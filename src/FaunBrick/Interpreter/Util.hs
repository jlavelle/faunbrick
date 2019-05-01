module FaunBrick.Interpreter.Util where

import Data.Word (Word8)
import qualified Data.Vector.Generic.Mutable as MV
import qualified Data.IntMap.Strict as M
import System.IO (stdout, stdin)

import FaunBrick.Interpreter.Types

defaultIntMapMem :: IntMapMem
defaultIntMapMem = IntMapMem M.empty 0

defaultMVecMem :: IO MVecMem
defaultMVecMem = do
  v <- MV.replicate 31000 0
  pure $ MVecMem v 1000

defaultTape :: Tape Word8
defaultTape = Tape (replicate 1000 0) 0 $ replicate 29999 0

defaultTextHandle :: TextHandle
defaultTextHandle = TextHandle mempty mempty

defaultIOHandle :: IOHandle
defaultIOHandle = IOHandle
  { ioHandleIn  = stdin
  , ioHandleOut = stdout
  }
