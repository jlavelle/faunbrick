module FaunBrick.AST.Util where

import Prelude hiding (replicate)

import Control.Applicative ((<|>))
import Control.Monad ((>=>))
import Data.Data (toConstr)
import Data.Functor.Foldable (cata)
import Data.Monoid (Sum(..))

import FaunBrick.AST

replicate :: (Ord a, Num a) => a -> b -> FaunBrick b
replicate n b | n <= 0    = Halt
              | otherwise = Instr b $ replicate (n - 1) b

groupBy :: (a -> a -> Bool) -> FaunBrick a -> FaunBrick (FaunBrick a)
groupBy _ Halt = Halt
groupBy p' (Loop as bs) = Loop (groupBy p' as) (groupBy p' bs)
groupBy p' (If as bs) = If (groupBy p' as) (groupBy p' bs)
groupBy p' (Instr x' xs') = Instr (Instr x' ys') zs'
  where
    (ys', zs') = go p' x' xs'
    go p z (Instr x xs)
      | p z x = (Instr x ys, zs)
      | otherwise = (Halt, Instr (Instr x ys) zs)
      where
        (ys, zs) = go p x xs
    go _ _ Halt = (Halt, Halt)
    go _ _ (Loop as bs) = (Halt, Loop (groupBy p' as) (groupBy p' bs))
    go _ _ (If as bs) = (Halt, If (groupBy p' as) (groupBy p' bs))

group :: Eq a => FaunBrick a -> FaunBrick (FaunBrick a)
group = groupBy (==)

-- Apply a transformation to each block of instructions
cataBlocks :: (a -> a -> Bool) -> (FaunBrick a -> FaunBrick a) -> FaunBrick a -> FaunBrick a
cataBlocks pr f = cata go . groupBy pr
  where
    go HaltF = Halt
    go (LoopF as bs) = Loop as bs
    go (IfF as bs) = If as bs
    go (InstrF p r) = f p <> r

last :: FaunBrick a -> Maybe a
last = cata go
  where
    go HaltF = Nothing
    go (InstrF a Nothing) = Just a
    go (InstrF _ a) = a
    go (LoopF a b) = b <|> a
    go (IfF a b) = b <|> a

single :: a -> FaunBrick a
single a = Instr a Halt

instrSum :: Program -> Int
instrSum = getSum . foldMap (Sum . instrVal)

instrVal :: Instruction -> Int
instrVal = \case
  Update _ n -> n
  Jump n -> n
  Set _ n -> n
  MulUpdate _ _ n -> n
  MulSet _ _ n -> n
  _ -> 0

addOffset :: Int -> Instruction -> Instruction
addOffset x = \case
  Output o -> Output $ o + x
  Input o  -> Input $ o + x
  Update o n -> Update (o + x) n
  Set o n -> Set (o + x) n
  MulUpdate s d n -> MulUpdate (s + x) (d + x) n
  MulSet s d n -> MulSet (s + x) (d + x) n
  Jump n -> Jump n

-- Eq that ignores the numeric values contained in the instructions and compares offsets
instrEq :: Instruction -> Instruction -> Bool
instrEq a b = toConstr a == toConstr b && offsets a == offsets b

offsets :: Instruction -> Maybe (Either Int (Int, Int))
offsets = \case
  Output o -> Just $ Left o
  Input o -> Just $ Left o
  Update o _ -> Just $ Left o
  Set o _ -> Just $ Left o
  MulUpdate s d _ -> Just $ Right (s, d)
  MulSet s d _ -> Just $ Right (s, d)
  Jump _ -> Nothing

srcOffset :: Instruction -> Maybe Int
srcOffset = offsets >=> either (const Nothing) (Just . fst)

updateOrJump :: Program -> Maybe Program
updateOrJump = cata go
  where
    go :: FaunBrickF Instruction (Maybe Program) -> Maybe Program
    go HaltF = Just Halt
    go (InstrF i@(Update _ _) r) = Instr i <$> r
    go (InstrF i@(Jump _) r) = Instr i <$> r
    go _ = Nothing

-- Checks if a program is suitable for the loopToIf optimization
ifable :: Program -> Maybe Program
ifable = cata go
  where
    go :: FaunBrickF Instruction (Maybe Program) -> Maybe Program
    go HaltF = Just Halt
    go (InstrF i@(ifelem -> True) r) = Instr i <$> r
    go _ = Nothing
    ifelem = \case
      Update _ _ -> True
      Set n _ -> n /= 0
      MulSet _ n _ -> n /= 0
      MulUpdate _ n _ -> n /= 0
      _ -> False
