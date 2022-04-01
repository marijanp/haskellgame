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
import Reactive.Banana.Combinators (Event, Behavior, accumE, accumB, mapAccum, (<@>), (<@), stepper, unionWith)

import Data.Map ((!))
import qualified Data.Map as M


import UnliftIO.Exception (bracket, bracket_)

import Linear.V2
import System.Clock
import System.Random (initStdGen, uniform, StdGen)

import Graphics.Textures
import EventSource

import GameLoop

import Game.Character
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

        wolfRunningLeftTextures <- liftIO $ traverse loadBMP $ take 8 $ (\i -> "haskellgame/images/wolf/running/left/" ++ show i ++ ".bmp") <$> [0..]
        wolfRunningRightTextures <- liftIO $ traverse loadBMP $ take 8 $ (\i -> "haskellgame/images/wolf/running/right/" ++ show i ++ ".bmp") <$> [0..]
        wolfIdleLeftTextures <- liftIO $ traverse loadBMP $ take 8 $ (\i -> "haskellgame/images/wolf/idle/left/" ++ show i ++ ".bmp") <$> [0..]
        wolfIdleRightTextures <- liftIO $ traverse loadBMP $ take 8 $ (\i -> "haskellgame/images/wolf/idle/right/" ++ show i ++ ".bmp") <$> [0..]

        let playerTextureMap = M.fromList [ (RightDirection, M.fromList [(Idle, idleRightTextures), (Running, runningRightTextures)]),
                                            (LeftDirection, M.fromList [(Idle, idleLeftTextures), (Running, runningLeftTextures)])
                                          ]
            wolfTextureMap = M.fromList [ (RightDirection, M.fromList [(Idle, wolfIdleRightTextures), (Running, wolfRunningRightTextures)]),
                                          (LeftDirection, M.fromList [(Idle, wolfIdleLeftTextures), (Running, wolfRunningLeftTextures)]),
                                          (UpDirection, M.fromList [(Running, wolfRunningRightTextures)]),
                                          (DownDirection, M.fromList [(Running, wolfRunningLeftTextures)])
                                        ]
            characterTextureMap = M.fromList [(Soldier, playerTextureMap), (Wolf, wolfTextureMap)]

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

        network <- liftIO $ setupNetwork window glossState renderFrameEventSource animationTimeOutEventSource controlKeysEventSource timeOutEventSource characterTextureMap
        liftIO $ actuate network

        initialTime <- liftIO $ getTime Monotonic

        let periodicEvents = [
              (PeriodicEvent initialTime (TimeSpec 0 2000000) renderFrameEventSource),
              (PeriodicEvent initialTime (TimeSpec 2 0) timeOutEventSource),
              (PeriodicEvent initialTime (TimeSpec 0 50000000) animationTimeOutEventSource)]
            gameConfig = GameConfig (V2 1000 800) window controlKeysEventSource

        liftIO $ execGameLoop gameLoop gameConfig periodicEvents

  GLFW.terminate

setupNetwork :: Window -> GlossR.State -> EventSource() -> EventSource() -> EventSource ControlKeys -> EventSource () -> CharacterTextureMap -> IO EventNetwork
setupNetwork window glossState renderFrameEventSource animationStepEventSource controlKeysEventSource npcTimeOutEventSource textures = compile $ do
    rndmGen <- initStdGen

    renderFrameEvent <- fromAddHandler $ addHandler renderFrameEventSource
    animationStepEvent <- fromAddHandler $ addHandler animationStepEventSource
    controlKeyEvent <- fromAddHandler $ addHandler controlKeysEventSource
    npcTimeOutEvent <- fromAddHandler $ addHandler npcTimeOutEventSource

    let playerMoveEvent = (\controlKey player -> moveCharacter $ changeStatus controlKey player) <$> controlKeyEvent
        playerAnimationStepEvent = moveIdleCharacter <$ animationStepEvent
    playerPositionBehavior <- accumB (Character Soldier (V2 0 0) Idle RightDirection 0 (AnimationState 10 0)) $ unionWith (\_ animate -> animate) playerMoveEvent playerAnimationStepEvent

    (changeDirectionEvent, _) <- mapAccum rndmGen $ uniform <$ npcTimeOutEvent
    monsterDirectionBehavior <- stepper RightDirection changeDirectionEvent

    let monsterMoveEvent = (\direction monster -> wander (500, 400) 8 direction monster) <$> monsterDirectionBehavior <@ renderFrameEvent
        monsterAnimationStepEvent = moveAnyCharacter <$ animationStepEvent
    monsterPositionBehavior <- accumB (Character Wolf (V2 100 100) Running RightDirection 8 (AnimationState 8 0)) $ unionWith (\_ animate -> animate) monsterMoveEvent monsterAnimationStepEvent

    reactimate $ (renderFrame window glossState textures) <$> playerPositionBehavior <*> monsterPositionBehavior <@ renderFrameEvent

renderFrame :: Window -> GlossR.State -> CharacterTextureMap -> Character -> Character -> IO ()
renderFrame window glossState textures p m = do
  displayPicture (1000, 800) blue glossState 1.0 $ Pictures [renderCharacter textures p, renderCharacter textures m]
  renderGUI
  -- Render
  -- glClear GL_COLOR_BUFFER_BIT
  render
  openGL3RenderDrawData =<< getDrawData
  GLFW.swapBuffers window

renderCharacter :: CharacterTextureMap -> Character -> Picture
renderCharacter textures (Character characterType (V2 xPos yPos) status dir _ animState) = do
  let currentPicture = textures ! characterType ! dir ! status !! (fromIntegral $ _pictureIndex animState)
  translate xPos yPos currentPicture

-- renderMonster :: CharacterTextureMap -> Monster -> Picture
-- renderMonster textures (Monster (V2 xPos yPos) _) = do
--   let currentPicture = textures ! Wolf ! dir ! status !! fromEnum animStage
--   translate xPos yPos currentPicture

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
