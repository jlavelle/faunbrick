module FaunBrick.Interpreter where

import Data.Word (Word8)
import Control.Monad.Except (MonadError, runExceptT, ExceptT)
import Data.Functor.Identity (Identity, runIdentity)

import FaunBrick.Interpreter.Types (
  Memory(..),
  Handle(..),
  TextHandle,
  Tape,
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
interpret e h (Instr i r) = step e h i  >>= \(e', h') -> interpret e' h' r
interpret e h (Loop bs r) = loop bs e h >>= \(e', h') -> interpret e' h' r
  where
    loop xs e' h' = do
      c <- readCell e'
      if c == 0
        then pure (e', h')
        else interpret e' h' xs >>= uncurry (loop xs)

step :: InterpretM e h m => e -> h -> Instruction -> m (e, h)
step e h i = case i of
  Output 0 -> writeOutput e h
  Input 0 -> readInput e h
  Update 0 n -> (,h) <$> modifyCell e (+ fromIntegral n)
  Jump n -> (,h) <$> movePointer e (+ n)
  Set 0 n -> (,h) <$> writeCell e (fromIntegral n)
  MulUpdate 0 o n -> (,h) <$> mulUpdate e o n
  MulSet 0 o n -> (,h) <$> mulSet e o n
  _ -> error "Interpreter: interpreter does not support command offset optimization."

modifyCell :: (Memory e m, Monad m) => e -> (Cell e -> Cell e) -> m e
modifyCell e f = readCell e >>= writeCell e . f

mulUpdate :: (Monad m, Memory e m, Integral (Cell e)) => e -> Int -> Int -> m e
mulUpdate e o n = do
  c <- readCell e
  e1 <- movePointer e (+ o)
  e2 <- modifyCell e1 (\c' -> c' + c * fromIntegral n)
  movePointer e2 (subtract o)

mulSet :: (Monad m, Memory e m, Integral (Cell e)) => e -> Int -> Int -> m e
mulSet e o n = do
  c <- readCell e
  e1 <- movePointer e (+ o)
  e2 <- writeCell e1 (c * fromIntegral n)
  movePointer e2 (subtract o)

-- TODO: Memory should support this directly
offsetMod :: (Monad m, Memory e m, Integral (Cell e)) => e -> Int -> (Cell e -> Cell e) -> m e
offsetMod e o f = do
  c <- readCell e
  e1 <- movePointer e (+ o)
  e2 <- modifyCell e1 (+ f c)
  movePointer e2 (subtract o)

writeOutput :: InterpretM e h m => e -> h -> m (e, h)
writeOutput e h = fmap (e,) $ readCell e >>= output h

readInput :: InterpretM e h m => e -> h -> m (e, h)
readInput e h = input h >>= \(a, h') -> (,h') <$> writeCell e a

newtype Pure a = Pure { runPure :: ExceptT Error Identity a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadError Error
           )

interpretPure :: Tape Word8 -> TextHandle -> Program -> Either Error (Tape Word8, TextHandle)
interpretPure t h = runIdentity . runExceptT . runPure . interpret t h

interpretPure' :: Program -> Either Error (Tape Word8, TextHandle)
interpretPure' = interpretPure Util.defaultTape Util.defaultTextHandle

interpretIO :: MVecMem -> IOHandle -> Program -> IO (MVecMem, IOHandle)
interpretIO = interpret

interpretIO' :: Program -> IO (MVecMem, IOHandle)
interpretIO' p = Util.defaultMVecMem >>= \m -> interpretIO m Util.defaultIOHandle p
