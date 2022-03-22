{-# language BlockArguments #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}
module Main ( main ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Managed
import DearImGui
import DearImGui.OpenGL3
import DearImGui.GLFW
import DearImGui.GLFW.OpenGL
import Graphics.GL
import Graphics.UI.GLFW (Window)
import qualified Graphics.UI.GLFW as GLFW
import UnliftIO.Exception (bracket, bracket_)

main :: IO ()
main = do
  GLFW.setErrorCallback $ Just (\ e s -> putStrLn $ unwords [show e, show s])
  initialised <- GLFW.init
  unless initialised $ error "GLFW init failed"

  runManaged $ do
    managedWindow <- managed $ bracket
      (GLFW.createWindow 1000 800 "Hello, Dear ImGui!" Nothing Nothing)
      (maybe (return ()) GLFW.destroyWindow)
    case managedWindow of
      Nothing -> do
        error "GLFW createWindow failed"
      Just window -> do
        liftIO $ do
          GLFW.makeContextCurrent (Just window)
          GLFW.swapInterval 1

        -- Create an ImGui context
        _ <- managed $ bracket createContext destroyContext

        -- Initialize ImGui's GLFW backend
        _ <- managed_ $ bracket_ (glfwInitForOpenGL window True) glfwShutdown

        -- Initialize ImGui's OpenGL backend
        _ <- managed_ $ bracket_ openGL3Init openGL3Shutdown

        liftIO $ mainLoop window

  GLFW.terminate

mainLoop :: Window -> IO ()
mainLoop window = do
  -- Process the event loop
  GLFW.pollEvents
  close <- GLFW.windowShouldClose window
  unless close do

    -- Tell ImGui we're starting a new frame
    openGL3NewFrame
    glfwNewFrame
    newFrame

    -- Build the GUI
    bracket_ (begin "Hello, ImGui!") end do
      -- Add a text widget
      text "Hello, ImGui!"

      -- Add a button widget, and call 'putStrLn' when it's clicked
      clicking <- button "Clickety Click"
      when clicking $
        putStrLn "Ow!"

    renderFrame window

    mainLoop window

renderFrame :: Window -> IO ()
renderFrame window = do
  -- Render
  glClear GL_COLOR_BUFFER_BIT
  render
  openGL3RenderDrawData =<< getDrawData
  GLFW.swapBuffers window