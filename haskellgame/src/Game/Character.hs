module Game.Character where

import Linear.V2
import Game.Direction
import Game.ControlKeys (ControlKeys(..))

data CharacterType = Soldier
                   | Wolf
                   deriving (Eq, Ord, Show)

data CharacterStatus = Idle
                     | Running
                     deriving (Eq, Ord, Show)

data Character = Character {
  _characterType :: CharacterType,
  _characterPosition :: V2 Float,
  _characterStatus :: CharacterStatus,
  _characterDirection :: Direction,
  _characterSpeed :: Float,
  _characterAnimationState :: AnimationState
} deriving (Eq, Show)

data AnimationState = AnimationState {
  _numPictures :: Integer,
  _pictureIndex :: Integer
} deriving (Eq, Show)

nextAnimationState :: AnimationState -> AnimationState
nextAnimationState animationState@(AnimationState bound index) =
  let
    newPictureIndex = if index + 1 >= bound then 0 else index + 1
  in
    animationState { _pictureIndex = newPictureIndex }

moveCharacter :: Character -> Character
moveCharacter c@(Character _ pos Running LeftDirection v animState) = c { _characterPosition = (pos + V2 (-v) 0), _characterAnimationState = nextAnimationState animState }
moveCharacter c@(Character _ pos Running RightDirection v animState) = c { _characterPosition = (pos + V2 v 0), _characterAnimationState = nextAnimationState animState}
-- movePlayer p@(Player pos (Running UpDirection v)) = p { _pPosition = (pos + V2 v 0) }
-- movePlayer p@(Player pos (Running DownDirection v)) = p { _pPosition = (pos + V2 (-v) 0) }
moveCharacter c = c

moveIdleCharacter :: Character -> Character
moveIdleCharacter c@(Character _ _ Idle _ _ animState) = c { _characterAnimationState = nextAnimationState animState }
moveIdleCharacter c = c

moveAnyCharacter :: Character -> Character
moveAnyCharacter c = c { _characterAnimationState = nextAnimationState $ _characterAnimationState c }

changeStatus :: ControlKeys -> Character -> Character
changeStatus (ControlKeys _ _ True _) c = c { _characterStatus = Running, _characterDirection = LeftDirection, _characterSpeed = 8 }
changeStatus (ControlKeys _ _ _ True) c = c { _characterStatus = Running, _characterDirection = RightDirection, _characterSpeed = 8 }
changeStatus _ c@(Character _ _ Running _ _ _) = c { _characterStatus = Idle, _characterSpeed = 0 }
changeStatus _ c = c


wander :: (Float, Float) -> Float -> Direction -> Character -> Character
wander bounds v UpDirection c =
  let newPosition = _characterPosition c + V2 0 v in
    if outOfBounds bounds newPosition
      then c { _characterDirection = DownDirection }
      else c { _characterPosition = newPosition }
wander bounds v DownDirection c =
  let newPosition = _characterPosition c + V2 0 (-v) in
    if outOfBounds bounds newPosition
      then c { _characterDirection = UpDirection }
      else c { _characterPosition = newPosition }
wander bounds v LeftDirection c =
  let newPosition = _characterPosition c + V2 (-v) 0 in
    if outOfBounds bounds newPosition
      then c { _characterDirection = RightDirection}
      else c { _characterPosition = newPosition }
wander bounds v RightDirection c =
  let newPosition = _characterPosition c + V2 v 0 in
    if outOfBounds bounds newPosition
      then c { _characterDirection = LeftDirection}
      else c { _characterPosition = newPosition }

outOfBounds :: (Float, Float) -> V2 Float -> Bool
outOfBounds (xBound, yBound) (V2 xPos yPos) = xPos > xBound || xPos < (-xBound) || yPos > yBound || yPos < (-yBound)

-- uniformCharacter :: RandomGen g => (Float, Float) -> CharacterType -> g -> (Monster, g)
-- uniformCharacter t r g = let (xPos, g') = uniformR r g 
--                              (yPos, g'') = uniformR r g'
--                              (direction, g''') = uniform g''
--                          in (Character t (V2 xPos yPos) Running direction 8, (), g''')
