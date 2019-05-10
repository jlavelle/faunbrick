module FaunBrick.Interpreter.Util where

import Data.Primitive.Types (Prim)
import Data.Vector.Primitive.Mutable (IOVector)
import System.IO (stdout, stdin)
import qualified Data.Vector.Generic.Mutable as MV

import FaunBrick.Interpreter.IO (IOHandle(..))
import FaunBrick.Interpreter.Pure (TextHandle(..), Tape(..))

defaultIOVector :: (Num a, Prim a) => IO (IOVector a)
defaultIOVector = MV.replicate 31000 0

defaultIOHandle :: IOHandle a
defaultIOHandle = IOHandle
  { ioHandleIn  = stdin
  , ioHandleOut = stdout
  }

defaultTape :: Num a => Tape (Int, a)
defaultTape = Tape (zip [0..] $ replicate 1000 0) (999, 0) (zip [1000..] $ replicate 29999 0)

defaultTextHandle :: TextHandle a
defaultTextHandle = TextHandle mempty mempty
