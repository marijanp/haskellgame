module Game.Player where

import Linear.V2

import Game.ControlKeys (ControlKeys(..))
import Game.Direction (Direction(..))

data PlayerStatus = Idle
                  | Running
                  deriving (Eq, Ord, Show)

data AnimationStage = One
                    | Two
                    | Three
                    | Four
                    | Five
                    | Six
                    | Seven
                    | Eight
                    | Nine
                    | Ten
                    deriving (Show, Eq, Enum, Bounded)

circular :: (Eq a, Enum a, Bounded a) => a -> a
circular x = if x == maxBound then minBound else succ x

data Player = Player {
  _pPosition :: V2 Float,
  _pStatus :: PlayerStatus,
  _pDirection :: Direction,
  _pSpeed :: Float,
  _animationStage :: AnimationStage
} deriving (Show, Eq)

changeStatus :: ControlKeys -> Player -> Player
changeStatus (ControlKeys _ _ True _) p = p { _pStatus = Running, _pDirection = LeftDirection, _pSpeed = 8 }
changeStatus (ControlKeys _ _ _ True) p = p { _pStatus = Running, _pDirection = RightDirection, _pSpeed = 8 }
changeStatus _ p@(Player _ Running _ _ _) = p { _pStatus = Idle, _pSpeed = 0 }
changeStatus _ p = p

movePlayer :: Player -> Player
movePlayer p@(Player pos Running LeftDirection v animStage) = p { _pPosition = (pos + V2 (-v) 0), _animationStage = circular animStage }
movePlayer p@(Player pos Running RightDirection v animStage) = p { _pPosition = (pos + V2 v 0), _animationStage = circular animStage }
-- movePlayer p@(Player pos (Running UpDirection v)) = p { _pPosition = (pos + V2 v 0) }
-- movePlayer p@(Player pos (Running DownDirection v)) = p { _pPosition = (pos + V2 (-v) 0) }
movePlayer p = p

moveIdlePlayer :: Player -> Player
moveIdlePlayer p@(Player _ Idle _ _ animStage) = p { _animationStage = circular animStage }
moveIdlePlayer p = p

-- movePlayer :: Float -> ControlKeys -> Player -> Player
-- movePlayer v (ControlKeys True _ True _) (Player p _) = Player (p + V2 (-v) v) $ Running LeftDirection
-- movePlayer v (ControlKeys True _ _ True) (Player p) = Player (p + V2 v v) $ Running RightDirection
-- movePlayer v (ControlKeys True _ _ _) (Player p)    = Player (p + V2 0 v) $  Running UpDirection
-- movePlayer v (ControlKeys _ True True _) (Player p) = Player (p + V2 (-v) (-v)) $ Running LeftDirection
-- movePlayer v (ControlKeys _ True _ True) (Player p) = Player (p + V2 v (-v)) $ Running RightDirection
-- movePlayer v (ControlKeys _ True _ _) (Player p)    = Player (p + V2 0 (-v)) $ Running DownDirection
-- movePlayer v (ControlKeys _ _ True _) (Player p)    = Player (p + V2 (-v) 0) $ Running LeftDirection
-- movePlayer v (ControlKeys _ _ _ True) (Player p)    = Player (p + V2 v 0) $ Running RightDirection
-- movePlayer _ _ (Player p _) = Player p $ Idle