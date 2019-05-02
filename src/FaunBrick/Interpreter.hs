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
  MVecMem
  )
import qualified FaunBrick.Interpreter.Util as Util
import FaunBrick.AST (Program, FaunBrick(..), Instruction(..))

type InterpretM e h m =
  (Monad m, Memory e m, Handle h m, Integral (Out h), Integral (Cell e), Cell e ~ Out h)

interpret :: InterpretM e h m => e -> h -> Program -> m (e, h)
interpret e h Halt = pure (e, h)
interpret e h (Instr i r) = step e h i >>= \(e', h') -> interpret e' h' r
interpret e h (If bs r) = branch bs e h >>= \(e', h') -> interpret e' h' r
interpret e h (Loop bs r) = loop bs e h >>= \(e', h') -> interpret e' h' r
{-# INLINE interpret #-}

branch :: InterpretM e h m => Program -> e -> h -> m (e, h)
branch xs e h = do
  c <- readCell e 0
  if c == 0
    then pure (e, h)
    else interpret e h xs
{-# INLINE branch #-}

loop :: InterpretM e h m => Program -> e -> h -> m (e, h)
loop xs e h = do
  c <- readCell e 0
  if c == 0
    then pure (e, h)
    else interpret e h xs >>= \(e', h') -> loop xs e' h'
{-# INLINE loop #-}

step :: InterpretM e h m => e -> h -> Instruction -> m (e, h)
step e h i = case i of
  Output o -> writeOutput e h o
  Input o -> readInput e h o
  Update o n -> (,h) <$> modifyCell e o (+ fromIntegral n)
  Jump n -> (,h) <$> movePointer e (+ n)
  Set o n -> (,h) <$> writeCell e o (fromIntegral n)
  MulUpdate s d n -> (,h) <$> mulUpdate e s d n
  MulSet s d n -> (,h) <$> mulSet e s d n
{-# INLINE step #-}

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

readInput :: InterpretM e h m => e -> h -> Int -> m (e, h)
readInput e h o = input h >>= \(a, h') -> (,h') <$> writeCell e o a
{-# INLINE readInput #-}

newtype Pure a = Pure { runPure :: ExceptT Error Identity a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadError Error
           )

interpretPure :: IntMapMem -> TextHandle -> Program -> Either Error (IntMapMem, TextHandle)
interpretPure t h = runIdentity . runExceptT . runPure . interpret t h

interpretPure' :: Program -> Either Error (IntMapMem, TextHandle)
interpretPure' = interpretPure Util.defaultIntMapMem Util.defaultTextHandle

interpretIO :: MVecMem -> IOHandle -> Program -> IO (MVecMem, IOHandle)
interpretIO = interpret

interpretIO' :: Program -> IO (MVecMem, IOHandle)
interpretIO' p = Util.defaultMVecMem >>= \m -> interpretIO m Util.defaultIOHandle p
