{-# LANGUAGE DeriveGeneric #-}
module Game.Direction where

import GHC.Generics
import System.Random (Uniform)

data Direction = UpDirection
               | DownDirection
               | LeftDirection
               | RightDirection
               deriving (Bounded, Enum, Eq, Generic, Ord, Show)

instance Uniform Direction
