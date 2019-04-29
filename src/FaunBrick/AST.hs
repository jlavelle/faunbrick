{-# LANGUAGE TemplateHaskell #-}

module FaunBrick.AST where

import GHC.Exts (IsList(..))
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

data SumR f a = L a | R (f a)
  deriving (Functor, Foldable, Traversable, Eq, Show)

newtype ProgView a = ProgView { runProgView :: [SumR ProgView a] }
  deriving (Functor, Foldable, Traversable, Eq, Show)

instance IsList (FaunBrick a) where
  type Item (FaunBrick a) = SumR ProgView a
  toList Halt = []
  toList (Instr a r)  = L a : toList r
  toList (Loop as bs) = R (ProgView $ toList as) : toList bs

  fromList [] = Halt
  fromList (L a : xs) = Instr a $ fromList xs
  fromList (R v : xs) = Loop (fromList $ runProgView v) $ fromList xs

toProgView :: FaunBrick a -> ProgView a
toProgView = ProgView . toList

fromProgView :: ProgView a -> FaunBrick a
fromProgView = fromList . runProgView

data Instruction
  = Put
  | Get
  | Update Int
  | Jump Int
  | Clear
  | Mul Int Int
  deriving (Eq, Show, Generic)

instance NFData Instruction

type Program = FaunBrick Instruction
