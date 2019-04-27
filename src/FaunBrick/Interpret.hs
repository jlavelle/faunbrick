module FaunBrick.Interpret (interpret, foldFaunBrick) where

import Control.Foldl (FoldM(..))
import qualified Control.Foldl as F

import FaunBrick.AST (Brick(..), FaunBrick)
import FaunBrick.MonadFaun (MonadFaun(..), forward, backward, add, sub)

type FaunCtx e m = (Eq (Cell e), Num (Cell e), Num (Pointer e), MonadFaun e m)

interpret :: FaunCtx e m => e -> FaunBrick -> m e
interpret e = F.foldM (foldFaunBrick e)

foldFaunBrick :: FaunCtx e m => e -> FoldM m Brick e
foldFaunBrick e = FoldM step (pure e) pure

step :: FaunCtx e m => e -> Brick -> m e
step e' b = act e'
  where
    act = case b of
      Forward  -> forward
      Backward -> backward
      Add -> add
      Sub -> sub
      Put -> output
      Get -> input
      Loop bs -> loop bs

loop :: FaunCtx e m => FaunBrick -> e -> m e
loop bs e = do
  c <- readCurrentCell e
  if c == 0
    then pure e
    else interpret e bs >>= loop bs
