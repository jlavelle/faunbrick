{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE DeriveDataTypeable #-}

module FaunBrick.AST where

import Data.Data (Data)
import GHC.Exts (IsList(..))
import GHC.Generics (Generic, Generic1)
import Control.DeepSeq (NFData, NFData1)

import Data.Functor.Foldable.TH (makeBaseFunctor)

data FaunBrick a
  = Instr a (FaunBrick a)
  | Loop (FaunBrick a) (FaunBrick a)
  | If (FaunBrick a) (FaunBrick a)
  | Halt
  deriving (Functor, Foldable, Traversable, Eq, Show, Generic, Generic1, Data)

instance NFData a => NFData (FaunBrick a)
instance NFData1 FaunBrick

makeBaseFunctor ''FaunBrick

instance Semigroup (FaunBrick a) where
  Halt <> x = x
  x <> Halt = x
  Instr a r <> r' = Instr a $ r <> r'
  Loop x r <> r' = Loop x $ r <> r'
  If x r <> r' = If x $ r <> r'

instance Monoid (FaunBrick a) where
  mempty = Halt

data Sum3 f a = L a | M (f a) | R (f a)
  deriving (Functor, Foldable, Traversable, Eq, Ord, Show)

newtype ProgView a = ProgView { runProgView :: [Sum3 ProgView a] }
  deriving (Functor, Foldable, Traversable, Eq, Ord, Show)

instance IsList (FaunBrick a) where
  type Item (FaunBrick a) = Sum3 ProgView a
  toList Halt = []
  toList (Instr a r)  = L a : toList r
  toList (Loop as bs) = M (ProgView $ toList as) : toList bs
  toList (If as bs)   = R (ProgView $ toList as) : toList bs

  fromList [] = Halt
  fromList (L a : xs) = Instr a $ fromList xs
  fromList (M v : xs) = Loop (fromList $ runProgView v) $ fromList xs
  fromList (R v : xs) = If (fromList $ runProgView v) $ fromList xs

toProgView :: FaunBrick a -> ProgView a
toProgView = ProgView . toList

fromProgView :: ProgView a -> FaunBrick a
fromProgView = fromList . runProgView

type Offset = Int
type Source = Offset
type Dest = Offset

data Instruction
  = Output Offset             -- output(m[p + offset])
  | Input Offset              -- input(m, p + offset)
  | Update Offset Int         -- m[p + offset] += v
  | Jump Int                  -- p += v
  | Set Offset Int            -- m[p + offset] = v
  | MulUpdate Source Dest Int -- m[p + dest] += m[p + source] * v
  | MulSet Source Dest Int    -- m[p + dest]  = m[p + source] * v
  deriving (Eq, Show, Generic, Data)

instance NFData Instruction

type Program = FaunBrick Instruction
