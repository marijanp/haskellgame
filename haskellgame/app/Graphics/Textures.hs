module Graphics.Textures where

import Data.Map as M

import Graphics.Gloss.Data.Picture (Picture)
import Game.Character
import Game.Character (CharacterType, CharacterStatus)
import Game.Direction (Direction)


type CharacterTextureMap = M.Map CharacterType (M.Map Direction (M.Map CharacterStatus [Picture]))
