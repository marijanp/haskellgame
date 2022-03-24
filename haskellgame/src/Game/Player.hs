module Game.Player where

import Linear.V2

import Game.ControlKeys (ControlKeys(..))

data Player = Player {
  _position :: V2 Float
} deriving (Show, Eq, Ord)

movePlayer :: Float -> ControlKeys -> Player -> Player
movePlayer v (ControlKeys True _ True _) (Player p) = Player $ p + V2 (-v) v
movePlayer v (ControlKeys True _ _ True) (Player p) = Player $ p + V2 v v
movePlayer v (ControlKeys True _ _ _) (Player p)    = Player $ p + V2 0 v
movePlayer v (ControlKeys _ True True _) (Player p) = Player $ p + V2 (-v) (-v)
movePlayer v (ControlKeys _ True _ True) (Player p) = Player $ p + V2 v (-v)
movePlayer v (ControlKeys _ True _ _) (Player p)    = Player $ p + V2 0 (-v)
movePlayer v (ControlKeys _ _ True _) (Player p)    = Player $ p + V2 (-v) 0
movePlayer v (ControlKeys _ _ _ True) (Player p)    = Player $ p + V2 v 0
movePlayer _ _ p = p