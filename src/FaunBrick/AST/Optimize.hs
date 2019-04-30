module FaunBrick.AST.Optimize where

import Data.Functor.Foldable (cata, embed, histo, Corecursive, Base)
import Control.Comonad.Cofree (Cofree(..))
import Control.Comonad (extract)
import Control.Monad ((>=>))
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as M

import FaunBrick.AST.Util (groupBy, single, instrSum, arithmetic, instrEq)
import FaunBrick.AST

type Optimization = Program -> Program

optimize :: Optimization
optimize = eqFix $ loopsToMul . elimClears . fuse . contract

-- contracts repeated Update/Jump/Sets at the same offset into single commands.
-- `instrEq` checks that the Instructions have the same constructor and offset.
contract :: Optimization
contract = cata go . groupBy instrEq
  where
    go :: FaunBrickF Program Program -> Program
    go HaltF = Halt
    go (LoopF as bs) = Loop as bs
    go (InstrF g r) = comb g <> r

    comb :: Program -> Program
    comb a@(Instr x _) = case x of
      Update o _ -> case instrSum a of
        0 -> Halt
        n -> single $ Update o n
      Jump _ -> single $ Jump $ instrSum a
      Set o _ -> single $ Set o $ instrSum a
      _ -> a
    comb _ = error "Optimizer: Unexpected input to comb."

-- Turns [-] and [+] into zero Sets
elimClears :: Optimization
elimClears = cata go
  where
    go :: FaunBrickF Instruction Program -> Program
    go (LoopF (Instr (Update o _) Halt) r) = Instr (Set o 0) r
    go x = embed x

-- Fuses together certain commands that operate on the same offset
fuse :: Optimization
fuse = histo go
  where
    go :: FaunBrickF Instruction (Cofree (FaunBrickF Instruction) Program) -> Program
    go HaltF = Halt

    -- updating instruction followed by a Set on the same offset does nothing; remove it.
    go x@(InstrF (fuseWithSet -> Just o) (r :< (InstrF (Set o' _) _)))
      | o == o' = r
      | otherwise = embedC x

    -- Input behaves the same way as Set
    go x@(InstrF (fuseWithSet -> Just o) (r :< (InstrF (Input o') _)))
      | o == o' = r
      | otherwise = embedC x

    -- If a Set is followed by an Update we can rewrite it as an increased Set
    go x@(InstrF (Set o n) (_ :< (InstrF (Update o' n') r)))
      | o == o' = Instr (Set o $ n + n') $ extract r
      | otherwise = embedC x

    -- Setting a cell to 0 and then MulUpdating it is a MulSet
    go x@(InstrF (Set o 0) (_ :< (InstrF (MulUpdate s d n) r)))
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

-- Rewrites suitable leaf loops as MulUpdate instructions
loopsToMul :: Optimization
loopsToMul = cata go
  where
    go :: FaunBrickF Instruction Program -> Program
    go (LoopF (arithmetic >=> simulate -> Just mul) r) = mul <> single (Set 0 0) <> r
    go x = embed x

    -- Run a program in an empty environment and see if it can be rewritten
    simulate :: Program -> Maybe Program
    simulate = analyze . foldl computeDelta (M.empty, 0)

    -- Computes the effect an instruction has on the environment
    computeDelta :: (IntMap Int, Int) -> Instruction -> (IntMap Int, Int)
    computeDelta (deltas, offset) = \case

      -- Updates change the value at the current offset
      Update o n ->
        let deltas' = M.insertWith (+) (offset + o) n deltas
        in (deltas', offset)

      -- Jumps move the offset
      Jump n -> (deltas, offset + n)

      -- This shouldn't happen if `arithmetic` is implemented properly.
      _ -> error "Optimizer: Unexpected input to loopsToMul.computeDelta"

    -- Try to build a program based on the contents of the environment
    analyze :: (IntMap Int, Int) -> Maybe Program
    analyze (deltas, offset)
      -- We can only optimize the loop if it writes the cell at offset 0 to -1
      -- and has a net offset of 0
      | offset /= 0 || M.findWithDefault 0 offset deltas /= (-1) = Nothing
      | otherwise = Just $ M.foldMapWithKey buildMul (M.delete 0 deltas)
      where
        -- Each entry in the deltas map corresponds to a MulUpdate at the
        -- entry's key
        buildMul off val = single $ MulUpdate 0 off val


embedC :: Corecursive t => Base t (Cofree (Base t) t) -> t
embedC = embed . fmap extract

eqFix :: Eq a => (a -> a) -> a -> a
eqFix f a = let r = f a in if r == a then r else eqFix f (f r)
