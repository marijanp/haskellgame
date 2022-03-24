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
-- import Graphics.GL
import Graphics.UI.GLFW (Window, Key(..), KeyState(..))
import qualified Graphics.UI.GLFW as GLFW

import Graphics.Gloss.Rendering (displayPicture, Point)
import Graphics.Gloss.Data.Color (red, blue)
import Graphics.Gloss.Data.Picture (rectangleSolid, color, translate)
import qualified Graphics.Gloss.Rendering as GlossR

import Control.Event.Handler (AddHandler, newAddHandler)
import Reactive.Banana (compile)
import Reactive.Banana.Frameworks (EventNetwork, actuate, reactimate, fromAddHandler)
import Reactive.Banana.Combinators (accumE)

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

        glossState <- liftIO $ GlossR.initState

        source <- liftIO $ newAddHandler

        network <- liftIO $ setupNetwork window glossState source
        liftIO $ actuate network

        liftIO $ mainLoop window source

  GLFW.terminate

mainLoop :: Window -> EventSource ControlKeys -> IO ()
mainLoop window controlKeysEventSource = do
  -- Process the event loop
  GLFW.pollEvents
  close <- GLFW.windowShouldClose window
  unless close do
    fire controlKeysEventSource =<< getControlKeys window
    esc <- keyIsPressed window Key'Escape
    if esc
      then return ()
      else mainLoop window controlKeysEventSource

type EventSource a = (AddHandler a, a -> IO ())

addHandler :: EventSource a -> AddHandler a
addHandler = fst

fire :: EventSource a -> a -> IO ()
fire = snd

setupNetwork :: Window -> GlossR.State -> EventSource ControlKeys -> IO EventNetwork
setupNetwork window glossState controlKeysEventSource = compile $ do
    controlKeyEvent <- fromAddHandler (addHandler controlKeysEventSource)
    playerMoveEvent <-  accumE (Player (0, 0)) $ (\controlKey player -> movePlayer 8 controlKey player) <$> controlKeyEvent
    reactimate $ (renderFrame window glossState) <$> playerMoveEvent


data Player = Player { _position :: Point } deriving (Show, Eq, Ord)

type Pressed = Bool

data ControlKeys = ControlKeys {
  _up :: Pressed,
  _down :: Pressed,
  _left :: Pressed,
  _right :: Pressed
} deriving (Eq, Ord, Show)

getControlKeys :: Window -> IO ControlKeys
getControlKeys window = do
  ControlKeys <$> keyIsPressed window Key'Up
              <*> keyIsPressed window Key'Down
              <*> keyIsPressed window Key'Left
              <*> keyIsPressed window Key'Right


movePlayer :: Float -> ControlKeys -> Player -> Player
movePlayer v (ControlKeys True _ True _) (Player (xPos, yPos)) = Player (xPos - v, yPos + v)
movePlayer v (ControlKeys True _ _ True) (Player (xPos, yPos)) = Player (xPos + v, yPos + v)
movePlayer v (ControlKeys True _ _ _) (Player (xPos, yPos)) = Player (xPos, yPos + v)
movePlayer v (ControlKeys _ True True _) (Player (xPos, yPos)) = Player (xPos - v, yPos - v)
movePlayer v (ControlKeys _ True _ True) (Player (xPos, yPos)) = Player (xPos + v, yPos - v)
movePlayer v (ControlKeys _ True _ _) (Player (xPos, yPos)) = Player (xPos, yPos - v)
movePlayer v (ControlKeys _ _ True _) (Player (xPos, yPos)) = Player (xPos - v, yPos)
movePlayer v (ControlKeys _ _ _ True) (Player (xPos, yPos)) = Player (xPos + v, yPos)
movePlayer _ _ p = p

renderFrame :: Window -> GlossR.State -> Player -> IO ()
renderFrame window glossState (Player (xPos, yPos)) = do
  displayPicture (1000, 800) blue glossState 5.0 $ translate xPos yPos (color red $ rectangleSolid 200 200)
  renderGUI
  -- Render
  -- glClear GL_COLOR_BUFFER_BIT
  render
  openGL3RenderDrawData =<< getDrawData
  GLFW.swapBuffers window

renderGUI:: IO ()
renderGUI = do
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

keyIsPressed :: Window -> Key -> IO Bool
keyIsPressed window key = keyStateIsPressed <$> GLFW.getKey window key

keyStateIsPressed :: KeyState -> Bool
keyStateIsPressed KeyState'Pressed = True
keyStateIsPressed KeyState'Repeating = True
keyStateIsPressed _ = False
