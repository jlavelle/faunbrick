module FaunBrick.AST.Optimize where

import Prelude hiding (last)

import Data.Maybe (maybe)
import Data.Functor.Foldable (cata, embed, histo, Corecursive, Base)
import Control.Comonad.Cofree (Cofree(..))
import Control.Comonad (extract)
import Control.Monad ((>=>))
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as Map
import Data.IntSet (IntSet)
import qualified Data.IntSet as Set

import FaunBrick.AST.Util (
  single,
  instrSum,
  updateOrJump,
  ifable,
  instrEq,
  addOffset,
  instrVal,
  last,
  cataBlocks,
  srcOffset
  )
import FaunBrick.AST

type Optimization = Program -> Program

optimize :: Optimization
optimize = eqFix
         $ dedupMulSet
         . loopsToIfs
         . uninterpose
         . offsets
         . elimClears
         . fuse
         . contract
         . loopsToMul

-- contracts repeated Update/Jump/Sets at the same offset into single commands.
-- `instrEq` checks that the Instructions have the same constructor and offset.
contract :: Optimization
contract = cataBlocks instrEq comb
  where
    comb :: Program -> Program
    comb a@(Instr x _) = case x of
      Update o _ -> case instrSum a of
        0 -> Halt
        n -> single $ Update o n
      Jump _ -> single $ Jump $ instrSum a
      MulUpdate s d _ -> single $ MulUpdate s d $ instrSum a
      MulSet s d n -> single $ MulSet s d $ maybe n instrVal (last a)
      Set o n -> single $ Set o $ maybe n instrVal (last a)
      _ -> a
    comb _ = error "Optimizer: Unexpected input to comb."

-- Turns [-] and [+] into zero Sets
elimClears :: Optimization
elimClears = cata go
  where
    go :: FaunBrickF Instruction Program -> Program
    go (LoopF (Instr (Update o _) Halt) r) = Instr (Set o 0) r
    go x = embed x

-- Fuses together certain commands that operate on the same offset or are redundant
fuse :: Optimization
fuse = histo go
  where
    go :: FaunBrickF Instruction (Cofree (FaunBrickF Instruction) Program) -> Program
    go HaltF = Halt

    -- updating instruction followed by a Set on the same offset does nothing; remove it.
    go x@(InstrF (fuseWithSet -> Just o) (r :< InstrF (Set o' _) _))
      | o == o' = r
      | otherwise = embedC x

    -- Input behaves the same way as Set
    go x@(InstrF (fuseWithSet -> Just o) (r :< InstrF (Input o') _))
      | o == o' = r
      | otherwise = embedC x

    -- MulSet behaves the same way as Set
    go x@(InstrF (fuseWithSet -> Just o) (r :< InstrF (MulSet s d _) _))
      | o == d && s /= d = r
      | otherwise = embedC x

    -- If a Set is followed by an Update we can rewrite it as an increased Set
    go x@(InstrF (Set o n) (_ :< InstrF (Update o' n') r))
      | o == o' = Instr (Set o $ n + n') $ extract r
      | otherwise = embedC x

    -- Setting a cell to 0 and then MulUpdating it is a MulSet
    go x@(InstrF (Set o 0) (_ :< InstrF (MulUpdate s d n) r))
      | o == d = Instr (MulSet s d n) $ extract r
      | otherwise = embedC x

    go x = embedC x

    fuseWithSet :: Instruction -> Maybe Offset
    fuseWithSet = \case
      Update o' _ -> Just o'
      Set o' _ -> Just o'
      MulSet _ d _ -> Just d
      MulUpdate _ d _ -> Just d
      _ -> Nothing

-- Eliminates the redundant 2nd instruction from this pattern:
-- m[p + x] = m[p + y] * 1
-- m[p + y] = m[p + x] * 1
dedupMulSet :: Optimization
dedupMulSet = histo go
  where
    go HaltF = Halt
    go x@(InstrF i@(MulSet s d 1) (_ :< InstrF (MulSet s' d' 1) r))
      | d == s' && s == d' = Instr i $ extract r
      | otherwise = embedC x
    go x = embedC x

-- After optimization, programs like hanoi.b end up with many subsequences in the form:
-- Set x 0
-- <unrelated instruction>
-- MulSet/MulUpdate/Update s x n
-- This optimization removes the Set x 0 and converts MulUpdates into MulSets if possible.
uninterpose :: Optimization
uninterpose = histo go
  where
    go :: FaunBrickF Instruction (Cofree (FaunBrickF Instruction) Program) -> Program
    go HaltF = Halt

    go x@(InstrF (Set o 0) (ra :< InstrF i@(srcOffset -> Just s) rb)) = case rb of

      _ :< InstrF (MulSet _ d' _) _
        | s /= o && d' == o -> ra
        | otherwise -> embedC x

      _ :< InstrF (MulUpdate s' d' n) r
        | s /= o && d' == o -> Instr i (Instr (MulSet s' d' n) (extract r))
        | otherwise -> embedC x

      -- Unclear whether or not this will ever happen
      _ :< InstrF (Update o' n) r
        | s /= o && o' == o -> Instr i (Instr (Set o' n) (extract r))
        | otherwise -> embedC x
      _ -> embedC x

    go x = embedC x

-- Rewrites suitable leaf loops as MulUpdate instructions
loopsToMul :: Optimization
loopsToMul = cata go
  where
    go :: FaunBrickF Instruction Program -> Program
    go (LoopF (updateOrJump >=> simulate -> Just mul) r) = mul <> single (Set 0 0) <> r
    go x = embed x

    -- Run a program in an empty environment and see if it can be rewritten
    simulate :: Program -> Maybe Program
    simulate = analyze . foldl computeDelta (Map.empty, 0)

    -- Computes the effect an instruction has on the environment
    computeDelta :: (IntMap Int, Int) -> Instruction -> (IntMap Int, Int)
    computeDelta (deltas, offset) = \case

      -- Updates change the value at the current offset + their offset
      Update o n ->
        let deltas' = Map.insertWith (+) (offset + o) n deltas
        in (deltas', offset)

      -- Jumps move the offset
      Jump n -> (deltas, offset + n)

      -- This shouldn't happen if `updateOrJump` is implemented properly.
      _ -> error "Optimizer: Unexpected input to loopsToMul.computeDelta"

    -- Try to build a program based on the contents of the environment
    analyze :: (IntMap Int, Int) -> Maybe Program
    analyze (deltas, offset)
      -- We can only optimize the loop if it writes the cell at offset 0 to -1
      -- and has a net offset of 0
      | offset /= 0 || Map.findWithDefault 0 offset deltas /= (-1) = Nothing
      | otherwise = Just $ Map.foldrWithKey buildMul Halt (Map.delete 0 deltas)
      where
        -- Each entry in the deltas map corresponds to a MulUpdate at the
        -- entry's key
        buildMul off val = Instr (MulUpdate 0 off val)

-- Delays pointer movement to the end of a block by computing the offset
-- that each instruction operates on.
offsets :: Optimization
offsets = cataBlocks (\_ _ -> True) go
  where
    go program =
      let (program', offset) = foldl computeOffset (mempty, 0) program
          jump = if offset == 0 then Halt else single (Jump offset)
      in program' <> jump

    computeOffset :: (Program, Int) -> Instruction -> (Program, Int)
    computeOffset (program, offset) = \case
      Jump n -> (program, offset + n)
      x ->
        let program' = program <> single (addOffset offset x)
        in (program', offset)

loopsToIfs :: Optimization
loopsToIfs = cata go
  where
    go :: FaunBrickF Instruction Program -> Program
    go (LoopF (ifable >=> simulate -> Just p) r) = If (p <> single (Set 0 0)) r
    go x = embed x

    simulate :: Program -> Maybe Program
    simulate = analyze . foldl computeClears (Set.singleton 0, Halt, 0)

    computeClears :: (IntSet, Program, Int) -> Instruction -> (IntSet, Program, Int)
    computeClears (clears, prog, origin) = \case
      Update o n | o == 0    -> (clears, prog, origin + n)
                 | otherwise ->
                   let prog' = prog <> single (MulUpdate 0 o n)
                       clears' = Set.delete o clears
                   in (clears', prog', origin)

      i@(MulUpdate _ d _) -> (Set.delete d clears, prog <> single i, origin)
      i@(MulSet _ d _)    -> (Set.delete d clears, prog <> single i, origin)

      i@(Set o n) -> let clears' | n == 0    = Set.insert o clears
                                 | otherwise = Set.delete o clears
                         prog' = prog <> single i
                     in (clears', prog', origin)

      _ -> error "Optimizer: Unexpected input to loopsToIfs.computeClears"

    analyze :: (IntSet, Program, Int) -> Maybe Program
    analyze (clears, prog, -1) = cata check prog
      where
        check HaltF = Just Halt
        check (InstrF i@(cleared -> True) r) = Instr i <$> r
        check _ = Nothing

        cleared = \case
          MulSet s _ _    -> Set.member s clears
          MulUpdate s _ _ -> Set.member s clears
          _ -> True

    analyze _ = Nothing

embedC :: Corecursive t => Base t (Cofree (Base t) t) -> t
embedC = embed . fmap extract

eqFix :: Eq a => (a -> a) -> a -> a
eqFix f a = let r = f a in if r == a then r else eqFix f (f r)
