{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances  #-}

module FaunBrick.AST (Brick(..), FaunBrick) where

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)

type FaunBrick = [Brick]

data Brick
  = Forward
  | Backward
  | Add
  | Sub
  | Put
  | Get
  | Loop [Brick]
  deriving (Show, Generic, Eq)

instance NFData Brick
