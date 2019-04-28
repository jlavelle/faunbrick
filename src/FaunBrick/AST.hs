{-# LANGUAGE TemplateHaskell #-}

module FaunBrick.AST (
  FaunBrick(..),
  FaunBrickF(..),
  Instruction(..),
  Program
) where

import GHC.Generics (Generic, Generic1)
import Control.DeepSeq (NFData, NFData1)

import Data.Functor.Foldable.TH (makeBaseFunctor)

data FaunBrick a
  = Instr a (FaunBrick a)
  | Loop (FaunBrick a) (FaunBrick a)
  | Halt
  deriving (Functor, Foldable, Traversable, Eq, Show, Generic, Generic1)

instance NFData a => NFData (FaunBrick a)
instance NFData1 FaunBrick

makeBaseFunctor ''FaunBrick

instance Semigroup (FaunBrick a) where
  Halt <> x = x
  x <> Halt = x
  Instr a r <> r' = Instr a $ r <> r'
  Loop x r <> r' = Loop x $ r <> r'

instance Monoid (FaunBrick a) where
  mempty = Halt

data Instruction
  = Forward
  | Backward
  | Add
  | Sub
  | Put
  | Get
  | Update Int
  | Jump Int
  | Clear
  deriving (Eq, Show, Generic)

instance NFData Instruction

type Program = FaunBrick Instruction
