module FaunBrick.Interpreter.Util where

import Data.Word (Word8)
import qualified Data.Vector.Generic.Mutable as MV
import System.IO (stdout, stdin)

import FaunBrick.Interpreter.Types

defaultMVecMem :: IO MVecMem
defaultMVecMem = do
  v <- MV.replicate 30000 0
  pure $ MVecMem v 0

defaultTape :: Tape Word8
defaultTape = Tape [] 0 $ replicate 29999 0

defaultTextHandle :: TextHandle
defaultTextHandle = TextHandle mempty mempty

defaultIOHandle :: IOHandle
defaultIOHandle = IOHandle stdout stdin
