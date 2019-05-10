module FaunBrick.Interpreter where

import Control.Monad (void)
import Control.Monad.Except (Except, liftEither, runExcept)
import Data.Primitive.Types (Prim)
import Data.Word (Word8, Word16, Word32, Word64)
import Streaming (Stream, Of)
import qualified Streaming.Prelude as S

import FaunBrick.AST (Program, FaunBrick(..), Instruction(..))
import FaunBrick.Common.Types (EofMode(..), BitWidth(..), Error(..))
import FaunBrick.Interpreter.Pure (Tape, TextHandle)
import FaunBrick.Interpreter.Types
import qualified FaunBrick.Interpreter.Util as Util

type CtxM m e h a =
  (Monad m, Integral a, Memory e m a, Handle h m a)

type CtxP e h a =
  (Integral a, PureMemory e a, PureHandle h a)

{-# INLINE interpretM #-}
interpretM :: forall m e h a. CtxM m e h a => EofMode -> e -> h -> Program -> Int -> m ()
interpretM eofMode mem hand prog off = void $ go prog off
  where
    go :: Program -> Int -> m Int
    go p !x = case p of
      Halt      -> pure x
      Instr i r -> step x i r
      If bs r   -> branch x bs r
      Loop bs r -> loop x bs r

    branch :: Int -> Program -> Program -> m Int
    branch x bs r = do
      c <- readCell mem x
      if c == 0 then go r x
                else go bs x >>= \x' -> go r x'

    loop :: Int -> Program -> Program -> m Int
    loop x bs r = do
      c <- readCell mem x
      if c == 0 then go r x
                else go bs x >>= \x' -> go (Loop bs r) x'

    step :: Int -> Instruction -> Program -> m Int
    step x i r = case i of
      Output o -> do
        c <- readCell mem (x + o)
        output hand c
        go r x
      Input o -> do
        mc <- input hand
        case mc of
          Just c -> writeCell mem (x + o) c *> go r x
          Nothing -> case eofMode of
            NoChange -> go r x
            MinusOne -> writeCell mem (x + o) (-1) *> go r x
            Zero     -> writeCell mem (x + o) 0    *> go r x
      Update o n -> do
        modifyCell mem (x + o) (\y -> y + fromIntegral n)
        go r x
      Jump n -> go r (x + n)
      Set o n -> do
        writeCell mem (x + o) (fromIntegral n)
        go r x
      MulUpdate s d n -> do
        c <- readCell mem (x + s)
        modifyCell mem (x + d) (\y -> y + (c * fromIntegral n))
        go r x
      MulSet s d n -> do
        c <- readCell mem (x + s)
        writeCell mem (x + d) (c * fromIntegral n)
        go r x

type PureStream e h = Stream (Of Char) (Except Error) (e, h, Int)

{-# INLINE interpretP #-}
interpretP :: forall e h a. CtxP e h a
           => EofMode
           -> e
           -> h
           -> Program
           -> Int
           -> PureStream e h
interpretP eofMode mem hand prog off = go mem hand prog off
  where
    go :: e -> h -> Program -> Int -> PureStream e h
    go e h p !x = case p of
      Halt      -> pure (e, h, x)
      Instr i r -> step e h x i r
      If bs r   -> branch e h x bs r
      Loop bs r -> loop e h x bs r

    branch :: e -> h -> Int -> Program -> Program -> PureStream e h
    branch e h x bs r = do
      c <- liftEither $ readCellP e x
      if c == 0 then go e h r x
                else go e h bs x >>= \(e', h', x') -> go e' h' r x'

    loop :: e -> h -> Int -> Program -> Program -> PureStream e h
    loop e h x bs r = do
      c <- liftEither $ readCellP e x
      if c == 0 then go e h r x
                else go e h bs x >>= \(e', h', x') -> go e' h' (Loop bs r) x'

    step :: e -> h -> Int -> Instruction -> Program -> PureStream e h
    step e h x i r = case i of
      Output o -> do
        c <- liftEither $ readCellP e (x + o)
        S.yield $ toEnum . fromEnum $ c
        let h' = outputP h c
        go e h' r x
      Input o -> do
        let (mc, h') = inputP h
        case mc of
          Just c -> liftEither (writeCellP e (x + o) c) >>= \e' -> go e' h' r x
          Nothing -> case eofMode of
            NoChange -> go e h' r x
            MinusOne -> liftEither (writeCellP e (x + o) (-1)) >>= \e' -> go e' h' r x
            Zero     -> liftEither (writeCellP e (x + o) 0)    >>= \e' -> go e' h' r x
      Update o n -> do
        e' <- liftEither $ modifyCellP e (x + o) (\y -> y + fromIntegral n)
        go e' h r x
      Jump n -> go e h r (x + n)
      Set o n -> do
        e' <- liftEither $ writeCellP e (x + o) (fromIntegral n)
        go e' h r x
      MulUpdate s d n -> do
        c <- liftEither $ readCellP e (x + s)
        e' <- liftEither $ modifyCellP e (x + d) (\y -> y + (c * fromIntegral n))
        go e' h r x
      MulSet s d n -> do
        c <- liftEither $ readCellP e (x + s)
        e' <- liftEither $ writeCellP e (x + d) (c * fromIntegral n)
        go e' h r x

interpretIO :: forall a. (Integral a, Packable a, Prim a)
            => EofMode
            -> Program
            -> IO ()
interpretIO m p = Util.defaultIOVector @a >>= \v -> interpretM m v Util.defaultIOHandle p 1000
{-# INLINE interpretIO #-}

interpretPureOut :: forall a. (Integral a, Packable a)
              => EofMode
              -> Program
              -> Either Error (Tape a, TextHandle a)
interpretPureOut m p = runExcept . S.effects $ do
  (e, h, _) <- interpretP m Util.defaultTape Util.defaultTextHandle p 999
  pure (snd <$> e, h)
{-# INLINE interpretPureOut #-}

runInterpretIO :: EofMode -> BitWidth -> Program -> IO ()
runInterpretIO m b p = case b of
  Width8  -> interpretIO @Word8 m p
  Width16 -> interpretIO @Word16 m p
  Width32 -> interpretIO @Word32 m p
  Width64 -> interpretIO @Word64 m p
