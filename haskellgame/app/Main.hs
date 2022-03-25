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

import Graphics.Gloss.Rendering (displayPicture)
import Graphics.Gloss.Data.Color (red, blue, green)
import Graphics.Gloss.Data.Picture (Picture(..), rectangleSolid, circleSolid, color, translate)
import Graphics.Gloss.Data.Bitmap (loadBMP)
import qualified Graphics.Gloss.Rendering as GlossR

import Control.Event.Handler (AddHandler, newAddHandler)
import Reactive.Banana (compile)
import Reactive.Banana.Frameworks (EventNetwork, actuate, reactimate, fromAddHandler)
import Reactive.Banana.Combinators (Event, Behavior, accumE, accumB, mapAccum, (<@>), (<@), stepper)

import Data.Map ((!))
import qualified Data.Map as M


import UnliftIO.Exception (bracket, bracket_)
import Control.Monad.State (StateT)
import Control.Monad.Reader (ReaderT)

import Linear.V2
import System.Clock
import System.Random (initStdGen, uniform, StdGen)

import Graphics.Textures

import Game.Player
import Game.Monster (Monster(..), MonsterStatus(..), wander)
import Game.ControlKeys
import Game.Direction

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

        idleRightTextures <- liftIO $ traverse loadBMP $ take 10 $ (\i -> "haskellgame/images/soldier/idle/right/" ++ show i ++ ".bmp") <$> [0..]
        runningRightTextures <- liftIO $ traverse loadBMP $ take 10 $ (\i -> "haskellgame/images/soldier/running/right/" ++ show i ++ ".bmp") <$> [0..]
        idleLeftTextures <- liftIO $ traverse loadBMP $ take 10 $ (\i -> "haskellgame/images/soldier/idle/left/" ++ show i ++ ".bmp") <$> [0..]
        runningLeftTextures <- liftIO $ traverse loadBMP $ take 10 $ (\i -> "haskellgame/images/soldier/running/left/" ++ show i ++ ".bmp") <$> [0..]

        let playerTextureMap = M.fromList [ (RightDirection, M.fromList [(Idle, idleRightTextures), (Running, runningRightTextures)]),
                                            (LeftDirection, M.fromList [(Idle, idleLeftTextures), (Running, runningLeftTextures)])
                                          ]

        -- Create an ImGui context
        _ <- managed $ bracket createContext destroyContext

        -- Initialize ImGui's GLFW backend
        _ <- managed_ $ bracket_ (glfwInitForOpenGL window True) glfwShutdown

        -- Initialize ImGui's OpenGL backend
        _ <- managed_ $ bracket_ openGL3Init openGL3Shutdown

        glossState <- liftIO $ GlossR.initState

        renderFrameEventSource <- liftIO $ newAddHandler
        animationTimeOutEventSource <- liftIO $ newAddHandler
        controlKeysEventSource <- liftIO $ newAddHandler
        timeOutEventSource <- liftIO $ newAddHandler

        network <- liftIO $ setupNetwork window glossState renderFrameEventSource animationTimeOutEventSource controlKeysEventSource timeOutEventSource playerTextureMap
        liftIO $ actuate network

        initialTime <- liftIO $ getTime Monotonic

        liftIO $ mainLoop initialTime window renderFrameEventSource animationTimeOutEventSource controlKeysEventSource timeOutEventSource

  GLFW.terminate

mainLoop :: TimeSpec -> Window -> EventSource () -> EventSource () -> EventSource ControlKeys -> EventSource () -> IO ()
mainLoop pastTime window renderFrameEventSource animationTimeOutEventSource controlKeysEventSource timeOutEventSource = do
  -- Process the event loop
  GLFW.pollEvents
  close <- GLFW.windowShouldClose window
  unless close do
    fire controlKeysEventSource =<< getControlKeys window
    currentTime <- getTime Monotonic
    let triggerRenderEvent = (currentTime - pastTime > (TimeSpec 0 2000000)) -- 30 FPS
    when triggerRenderEvent $ fire renderFrameEventSource ()
    esc <- keyIsPressed window Key'Escape
    if esc
      then return ()
      else mainLoop (if triggerRenderEvent then currentTime else pastTime) window renderFrameEventSource animationTimeOutEventSource controlKeysEventSource timeOutEventSource
    

type EventSource a = (AddHandler a, a -> IO ())

addHandler :: EventSource a -> AddHandler a
addHandler = fst

fire :: EventSource a -> a -> IO ()
fire = snd

setupNetwork :: Window -> GlossR.State -> EventSource() -> EventSource() -> EventSource ControlKeys -> EventSource () -> PlayerTextureMap-> IO EventNetwork
setupNetwork window glossState renderFrameEventSource animationTimeOutEventSource controlKeysEventSource timeOutEventSource textures = compile $ do
    rndmGen <- initStdGen

    renderFrameEvent <- fromAddHandler $ addHandler renderFrameEventSource
    controlKeyEvent <- fromAddHandler (addHandler controlKeysEventSource)
    timeOutEvent <- fromAddHandler (addHandler timeOutEventSource)

    let playerMoveEvent = (\controlKey player -> moveIdlePlayer $ movePlayer $ changeStatus controlKey player) <$> controlKeyEvent
    playerPositionBehavior <- accumB (Player (V2 0 0) Idle RightDirection 0 One) playerMoveEvent

    (changeDirectionEvent, _) <- mapAccum rndmGen $ uniform <$ timeOutEvent
    monsterDirectionBehavior <- stepper RightDirection changeDirectionEvent

    let monsterMoveEvent = (\direction monster -> wander (500, 400) 2 direction monster) <$> monsterDirectionBehavior <@ controlKeyEvent
    monsterPositionBehavior <- accumB (Monster (V2 100 100) (Wander RightDirection)) $ monsterMoveEvent

    reactimate $ (renderFrame window glossState textures) <$> playerPositionBehavior <*> monsterPositionBehavior <@ renderFrameEvent

getControlKeys :: Window -> IO ControlKeys
getControlKeys window = do
  ControlKeys <$> keyIsPressed window Key'Up
              <*> keyIsPressed window Key'Down
              <*> keyIsPressed window Key'Left
              <*> keyIsPressed window Key'Right


renderFrame :: Window -> GlossR.State -> PlayerTextureMap -> Player -> Monster -> IO ()
renderFrame window glossState textures p m = do
  displayPicture (1000, 800) blue glossState 1.0 $ Pictures [renderPlayer textures p, renderMonster m]
  renderGUI
  -- Render
  -- glClear GL_COLOR_BUFFER_BIT
  render
  openGL3RenderDrawData =<< getDrawData
  GLFW.swapBuffers window

renderPlayer :: PlayerTextureMap -> Player -> Picture
renderPlayer textures (Player (V2 xPos yPos) status dir _ animStage) = do
  let currentPicture = textures ! dir ! status !! fromEnum animStage
  translate xPos yPos currentPicture

renderMonster :: Monster -> Picture
renderMonster (Monster (V2 xPos yPos) _) = do
  Color green $ translate xPos yPos $ circleSolid 100

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
