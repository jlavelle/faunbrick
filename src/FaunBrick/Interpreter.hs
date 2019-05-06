module FaunBrick.Interpreter where

import Control.Monad (void)
import Control.Monad.Except (MonadError, runExceptT, ExceptT)
import Data.Functor.Identity (Identity, runIdentity)
import Data.Primitive.Types (Prim)
import Data.Proxy (Proxy(..))
import Data.Word (Word8, Word16, Word32, Word64)

import FaunBrick.AST (Program, FaunBrick(..), Instruction(..))
import FaunBrick.Common.Types (EofMode(..), BitWidth(..))
import FaunBrick.Interpreter.Types
import qualified FaunBrick.Interpreter.Util as Util
import qualified FaunBrick.Interpreter.IO as IO

type InterpretM e h m =
  (Monad m, Memory e m, Handle h m, Integral (Out h), Integral (Cell e), Cell e ~ Out h)

{-# INLINE interpret #-}
interpret :: forall e h m. InterpretM e h m => EofMode -> e -> h -> Program -> m (e, h)
interpret eofMode mem hand prog = go prog mem hand
  where
    go :: Program -> e -> h -> m (e, h)
    go p e h = case p of
      Halt      -> pure (e, h)
      Instr i r -> step i r
      If bs r   -> branch bs r
      Loop bs r -> loop bs r

      where
        branch :: Program -> Program -> m (e, h)
        branch bs r = do
          c <- readCell e 0
          if c == 0 then go r e h
                    else go bs e h >>= \(e', h') -> go r e' h'

        loop :: Program -> Program -> m (e, h)
        loop bs r = do
          c <- readCell e 0
          if c == 0 then go r e h
                    else go bs e h >>= \(e', h') -> go (Loop bs r) e' h'

        step :: Instruction -> Program -> m (e, h)
        step i r =
          let a = case i of
                Output o        -> writeOutput e h o
                Input o         -> readInput eofMode e h o
                Update o n      -> (,h) <$> modifyCell e o (+ fromIntegral n)
                Jump n          -> (,h) <$> movePointer e (+ n)
                Set o n         -> (,h) <$> writeCell e o (fromIntegral n)
                MulUpdate s d n -> (,h) <$> mulUpdate e s d n
                MulSet s d n    -> (,h) <$> mulSet e s d n
          in a >>= \(e', h') -> go r e' h'

mulUpdate :: (Monad m, Memory e m, Integral (Cell e)) => e -> Int -> Int -> Int -> m e
mulUpdate e s d n = do
  c <- readCell e s
  modifyCell e d (+ (c * fromIntegral n))

mulSet :: (Monad m, Memory e m, Integral (Cell e)) => e -> Int -> Int -> Int -> m e
mulSet e s d n = do
  c <- readCell e s
  writeCell e d (c * fromIntegral n)

writeOutput :: InterpretM e h m => e -> h -> Int -> m (e, h)
writeOutput e h o = fmap (e,) $ readCell e o >>= output h

readInput :: InterpretM e h m => EofMode -> e -> h -> Int -> m (e, h)
readInput eof e h o = do
  (mi, h1) <- input h
  case mi of
    Just i -> (,h1) <$> writeCell e o i
    Nothing -> case eof of
      NoChange -> pure (e, h1)
      Zero     -> (,h1) <$> writeCell e o 0
      MinusOne -> (,h1) <$> writeCell e o (-1)

newtype Pure a = Pure { runPure :: ExceptT Error Identity a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadError Error
           )

interpretPure :: (Integral a, Packable a)
              => Tape a
              -> TextHandle a
              -> Program
              -> Either Error (Tape a, TextHandle a)
interpretPure t h = runIdentity . runExceptT . runPure . interpret NoChange t h
{-# INLINE interpretPure #-}

interpretPure' :: (Integral a, Packable a)
               => Program
               -> Either Error (Tape a, TextHandle a)
interpretPure' = interpretPure Util.defaultTape Util.defaultTextHandle
{-# INLINE interpretPure' #-}

interpretIO :: (Integral a, Packable a, Prim a)
             => EofMode
             -> Program
             -> Proxy a
             -> IO (MVecMem a, IOHandle a)
interpretIO em p _ = Util.defaultMVecMem >>= \m -> interpret em m Util.defaultIOHandle p
{-# INLINE interpretIO #-}

runInterpretIO :: EofMode -> BitWidth -> Program -> IO ()
runInterpretIO m b p = case b of
  Width8  -> do
    (MVecMem v _) <- Util.defaultMVecMem
    IO.interpret m v Util.defaultIOHandle p
  Width16 -> void $ interpretIO m p (Proxy @Word16)
  Width32 -> void $ interpretIO m p (Proxy @Word32)
  Width64 -> void $ interpretIO m p (Proxy @Word64)
