{-# language GeneralizedNewtypeDeriving #-}
module GameLoop where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (StateT, MonadState, runStateT, get, put, when)
import Control.Monad.Reader (ReaderT, MonadReader, runReaderT, asks)
import EventSource
import Linear.V2
import System.Clock

import Graphics.UI.GLFW (Window, Key(..))
import qualified Graphics.UI.GLFW as GLFW

import Game.ControlKeys (ControlKeys(..))

data PeriodicEvent = PeriodicEvent {
  _lastTime :: TimeSpec,
  _period :: TimeSpec,
  _eventSource :: EventSource ()
}

data PeriodicEvents = PeriodicEvents {
  _renderEvent :: PeriodicEvent,
  _animationEvent :: PeriodicEvent,
  _monsterDirectionEvent :: PeriodicEvent
}

data GameConfig = GameConfig {
  _windowDimensions :: V2 Float,
  _window :: Window,
  _controlKeysEventSource :: EventSource ControlKeys
}
    
newtype GameLoop a = GameLoop {
  _runGameLoop :: ReaderT GameConfig (StateT [PeriodicEvent] IO) a
} deriving (Functor, Applicative, Monad, MonadIO, MonadReader GameConfig, MonadState [PeriodicEvent])

execGameLoop :: GameLoop a -> GameConfig -> [PeriodicEvent] -> IO a
execGameLoop gameLoop gameConfig periodicEvents = fst <$> runStateT (runReaderT (_runGameLoop gameLoop) gameConfig) periodicEvents

gameLoop :: GameLoop ()
gameLoop = do
  -- Process the event loop
  window <- asks _window
  liftIO $ GLFW.pollEvents
  close <- liftIO $ (||) <$> (GLFW.windowShouldClose window) <*> (keyIsPressed window GLFW.Key'Escape)
  if close 
    then return ()
  else do
    -- fire current controlkey assignment
    controlKeyAssignment <- liftIO $ getControlKeys window
    controKeyEvent <- asks _controlKeysEventSource
    liftIO $ fire controKeyEvent controlKeyAssignment

    -- fire periodic events
    currentTime <- liftIO $ getTime Monotonic
    periodicEvents <- get
    updatedPeriodicEvents <- liftIO $ traverse (fireIfTimeOut currentTime) periodicEvents
    put updatedPeriodicEvents

    gameLoop

  where
    keyIsPressed :: Window -> GLFW.Key -> IO Bool
    keyIsPressed window key = keyStateIsPressed <$> GLFW.getKey window key
    keyStateIsPressed :: GLFW.KeyState -> Bool
    keyStateIsPressed GLFW.KeyState'Pressed = True
    keyStateIsPressed GLFW.KeyState'Repeating = True
    keyStateIsPressed _ = False
    getControlKeys :: Window -> IO ControlKeys
    getControlKeys window = do
      ControlKeys <$> keyIsPressed window Key'Up
                  <*> keyIsPressed window Key'Down
                  <*> keyIsPressed window Key'Left
                  <*> keyIsPressed window Key'Right

fireIfTimeOut :: TimeSpec -> PeriodicEvent -> IO (PeriodicEvent)
fireIfTimeOut currentTime periodicEvent = do
  if shouldTriggerPeriodicEvent currentTime periodicEvent
    then  do
      fire (_eventSource periodicEvent) ()
      return $ periodicEvent { _lastTime = currentTime} 
    else
      return periodicEvent

shouldTriggerPeriodicEvent :: TimeSpec -> PeriodicEvent -> Bool
shouldTriggerPeriodicEvent currentTime e = currentTime - (_lastTime e)  > _period e