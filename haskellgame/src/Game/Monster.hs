module Game.Monster where

import Linear.V2
import System.Random (RandomGen, uniform, uniformR)

import Game.Direction (Direction(..))

data MonsterStatus = Wander Direction deriving (Eq, Show)

data Monster = Monster {
  _monsterPosition :: V2 Float,
  _monsterStatus :: MonsterStatus
} deriving (Eq, Show)

wander :: (Float, Float) -> Float -> Direction -> Monster -> Monster
wander bounds v UpDirection (Monster p s) =
  let newPosition = p + V2 0 v in
    if outOfBounds bounds newPosition
      then Monster p (Wander DownDirection)
      else Monster newPosition s
wander bounds v DownDirection (Monster p s) =
  let newPosition = p + V2 0 (-v) in
    if outOfBounds bounds newPosition
      then Monster p (Wander UpDirection)
      else Monster newPosition s
wander bounds v LeftDirection (Monster p s) =
  let newPosition = p + V2 (-v) 0 in
    if outOfBounds bounds newPosition
      then Monster p (Wander RightDirection)
      else Monster newPosition s
wander bounds v RightDirection (Monster p s) =
  let newPosition = p + V2 v 0 in
    if outOfBounds bounds newPosition
      then Monster p (Wander LeftDirection)
      else Monster newPosition s

outOfBounds :: (Float, Float) -> V2 Float -> Bool
outOfBounds (xBound, yBound) (V2 xPos yPos) = xPos > xBound || xPos < (-xBound) || yPos > yBound || yPos < (-yBound)

uniformMonster :: RandomGen g => (Float, Float) -> g -> (Monster, g)
uniformMonster r g = let (xPos, g') = uniformR r g 
                         (yPos, g'') = uniformR r g'
                         (direction, g''') = uniform g''
                     in (Monster (V2 xPos yPos) (Wander direction), g''')
