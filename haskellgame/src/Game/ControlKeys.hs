module Game.ControlKeys where

type Pressed = Bool

data ControlKeys = ControlKeys {
  _up :: Pressed,
  _down :: Pressed,
  _left :: Pressed,
  _right :: Pressed
} deriving (Eq, Ord, Show)

