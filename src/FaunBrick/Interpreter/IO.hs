module FaunBrick.Interpreter.IO where

import Control.Monad (void)
import Data.Vector.Primitive.Mutable (IOVector)
import Data.Word (Word8)
import System.IO (hGetChar, hPutChar)
import System.IO.Error (isEOFError, catchIOError)
import qualified Data.Vector.Generic.Mutable as MV

import FaunBrick.AST (Program, FaunBrick(..), Instruction(..))
import FaunBrick.Common.Types (EofMode(..))
import FaunBrick.Interpreter.Types (IOHandle(..), wordToChar, charToWord)

{-# INLINE interpret #-}
interpret :: EofMode -> IOVector Word8 -> IOHandle Word8 -> Program -> IO ()
interpret eofMode mem (IOHandle inp out) prog = void $ go prog 1000
  where
    go :: Program -> Int -> IO Int
    go p !x = case p of
      Halt      -> pure x
      Instr i r -> step x i r
      If bs r   -> branch x bs r
      Loop bs r -> loop x bs r

    branch :: Int -> Program -> Program -> IO Int
    branch x bs r = do
      c <- MV.unsafeRead mem x
      if c == 0 then go r x
                else go bs x >>= \x' -> go r x'

    loop :: Int -> Program -> Program -> IO Int
    loop x bs r = do
      c <- MV.unsafeRead mem x
      if c == 0 then go r x
                else go bs x >>= \x' -> go (Loop bs r) x'

    step :: Int -> Instruction -> Program -> IO Int
    step x i r = case i of
      Output o -> do
        c <- wordToChar <$> MV.unsafeRead mem (x + o)
        hPutChar out c
        go r x
      Input o -> do
        c <- catchIOError (charToWord <$> hGetChar inp) $ \e -> do
          if not $ isEOFError e then ioError e else case eofMode of
            NoChange -> MV.unsafeRead mem (x + o)
            MinusOne -> pure 255
            Zero     -> pure 0
        MV.unsafeWrite mem (x + o) c
        go r x
      Update o n -> do
        MV.modify mem (\y -> y + fromIntegral n) (x + o)
        go r x
      Jump n -> go r (x + n)
      Set o n -> do
        MV.unsafeWrite mem (x + o) (fromIntegral n)
        go r x
      MulUpdate s d n -> do
        c <- MV.unsafeRead mem (x + s)
        MV.modify mem (\y -> y + (c * fromIntegral n)) (x + d)
        go r x
      MulSet s d n -> do
        c <- MV.unsafeRead mem (x + s)
        MV.unsafeWrite mem (x + d) (c * fromIntegral n)
        go r x
