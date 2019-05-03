module FaunBrick.Interpreter.Util where

import qualified Data.Vector.Generic.Mutable as MV
import qualified Data.IntMap.Strict as M
import Data.Primitive.Types (Prim)
import System.IO (stdout, stdin)

import FaunBrick.Interpreter.Types

defaultIntMapMem :: IntMapMem a
defaultIntMapMem = IntMapMem M.empty 0

defaultMVecMem :: (Num a, Prim a) => IO (MVecMem a)
defaultMVecMem = do
  v <- MV.replicate 31000 0
  pure $ MVecMem v 1000

defaultTape :: Num a => Tape a
defaultTape = Tape (replicate 1000 0) 0 $ replicate 29999 0

defaultTextHandle :: TextHandle a
defaultTextHandle = TextHandle mempty mempty

defaultIOHandle :: IOHandle a
defaultIOHandle = IOHandle
  { ioHandleIn  = stdin
  , ioHandleOut = stdout
  }
