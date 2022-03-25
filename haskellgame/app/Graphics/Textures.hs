module Graphics.Textures where

import Data.Map as M

import Graphics.Gloss.Data.Picture (Picture)
import Game.Player (PlayerStatus)
import Game.Direction (Direction)


type PlayerTextureMap = M.Map Direction (M.Map PlayerStatus [Picture])
