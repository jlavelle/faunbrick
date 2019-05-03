module FaunBrick.Interpreter where

import Control.Monad.Except (MonadError, runExceptT, ExceptT)
import Data.Functor.Identity (Identity, runIdentity)

import FaunBrick.Interpreter.Types (
  Memory(..),
  Handle(..),
  TextHandle,
  IntMapMem,
  Error,
  IOHandle,
  MVecMem,
  EofMode(..)
  )
import qualified FaunBrick.Interpreter.Util as Util
import FaunBrick.AST (Program, FaunBrick(..), Instruction(..))

type InterpretM e h m =
  (Monad m, Memory e m, Handle h m, Integral (Out h), Integral (Cell e), Cell e ~ Out h)

interpret :: InterpretM e h m => EofMode -> e -> h -> Program -> m (e, h)
interpret eofMode e'' h'' p = go e'' h'' p
  where
    {-# INLINE go #-}
    go e h = \case
      Halt -> pure (e, h)
      Instr i r -> step i >>= \(e', h') -> go e' h' r
      If bs r -> branch bs >>= \(e', h') -> go e' h' r
      Loop bs r -> loop e h bs >>= \(e', h') -> go e' h' r
      where
        {-# INLINE branch #-}
        branch bs = do
          c <- readCell e 0
          if c == 0
            then pure (e, h)
            else go e h bs

        {-# INLINE loop #-}
        loop le lh bs = do
          c <- readCell le 0
          if c == 0
            then pure (le, lh)
            else go le lh bs >>= \(e', h') -> loop e' h' bs

        {-# INLINE step #-}
        step = \case
          Output o -> writeOutput e h o
          Input o -> readInput eofMode e h o
          Update o n -> (,h) <$> modifyCell e o (+ fromIntegral n)
          Jump n -> (,h) <$> movePointer e (+ n)
          Set o n -> (,h) <$> writeCell e o (fromIntegral n)
          MulUpdate s d n -> (,h) <$> mulUpdate e s d n
          MulSet s d n -> (,h) <$> mulSet e s d n

mulUpdate :: (Monad m, Memory e m, Integral (Cell e)) => e -> Int -> Int -> Int -> m e
mulUpdate e s d n = do
  c <- readCell e s
  modifyCell e d (+ (c * fromIntegral n))
{-# INLINE mulUpdate #-}

mulSet :: (Monad m, Memory e m, Integral (Cell e)) => e -> Int -> Int -> Int -> m e
mulSet e s d n = do
  c <- readCell e s
  writeCell e d (c * fromIntegral n)
{-# INLINE mulSet #-}

writeOutput :: InterpretM e h m => e -> h -> Int -> m (e, h)
writeOutput e h o = fmap (e,) $ readCell e o >>= output h
{-# INLINE writeOutput #-}

readInput :: InterpretM e h m => EofMode -> e -> h -> Int -> m (e, h)
readInput eof e h o = do
  (mi, h1) <- input h
  case mi of
    Just i -> (,h1) <$> writeCell e o i
    Nothing -> case eof of
      NoChange -> pure (e, h1)
      Zero     -> (,h1) <$> writeCell e o 0
      MinusOne -> (,h1) <$> writeCell e o (-1)
{-# INLINE readInput #-}

newtype Pure a = Pure { runPure :: ExceptT Error Identity a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadError Error
           )

interpretPure :: IntMapMem -> TextHandle -> Program -> Either Error (IntMapMem, TextHandle)
interpretPure t h = runIdentity . runExceptT . runPure . interpret NoChange t h

interpretPure' :: Program -> Either Error (IntMapMem, TextHandle)
interpretPure' = interpretPure Util.defaultIntMapMem Util.defaultTextHandle

interpretIO :: MVecMem -> IOHandle -> Program -> IO (MVecMem, IOHandle)
interpretIO = interpret NoChange

interpretIO' :: Program -> IO (MVecMem, IOHandle)
interpretIO' p = Util.defaultMVecMem >>= \m -> interpretIO m Util.defaultIOHandle p
