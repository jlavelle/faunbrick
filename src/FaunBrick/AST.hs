module FaunBrick.AST (Brick(..)) where

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)

data Brick
  = Forward
  | Backward
  | Add
  | Sub
  | Put
  | Get
  | Loop [Brick]
  deriving (Show, Generic)

instance NFData Brick
