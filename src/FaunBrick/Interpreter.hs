module FaunBrick.Interpreter where

import Control.Monad (void)
import Control.Monad.Except (MonadError, runExceptT, ExceptT)
import Data.Functor.Identity (Identity, runIdentity)
import Data.Primitive.Types (Prim)
import Data.Word (Word8, Word16, Word32, Word64)
import Data.Proxy (Proxy(..))

import FaunBrick.Interpreter.Types (
  Memory(..),
  Handle(..),
  Packable,
  TextHandle,
  Tape,
  Error,
  IOHandle,
  MVecMem,
  )
import qualified FaunBrick.Interpreter.Util as Util
import FaunBrick.AST (Program, FaunBrick(..), Instruction(..))
import FaunBrick.Common.Types (EofMode(..), BitWidth(..))

type InterpretM e h m =
  (Monad m, Memory e m, Handle h m, Integral (Out h), Integral (Cell e), Cell e ~ Out h)

{-# INLINE interpret #-}
interpret :: forall e h m. InterpretM e h m => EofMode -> e -> h -> Program -> m (e, h)
interpret eofMode e'' h'' p = go e'' h'' p
  where
    {-# INLINE go #-}
    go :: e -> h -> Program -> m (e, h)
    go e h = \case
      Halt      -> pure (e, h)
      Instr i r -> step i      >>= \(e', h') -> go e' h' r
      If bs r   -> branch bs   >>= \(e', h') -> go e' h' r
      Loop bs r -> loop e h bs >>= \(e', h') -> go e' h' r

      where
        {-# INLINE branch #-}
        branch :: Program -> m (e, h)
        branch bs = do
          c <- readCell e 0
          if c == 0 then pure (e, h)
                    else go e h bs

        {-# INLINE loop #-}
        loop :: e -> h -> Program -> m (e, h)
        loop le lh bs = do
          c <- readCell le 0
          if c == 0 then pure (le, lh)
                    else go le lh bs >>= \(e', h') -> loop e' h' bs

        {-# INLINE step #-}
        step :: Instruction -> m (e, h)
        step = \case
          Output o        -> writeOutput e h o
          Input o         -> readInput eofMode e h o
          Update o n      -> (,h) <$> modifyCell e o (+ fromIntegral n)
          Jump n          -> (,h) <$> movePointer e (+ n)
          Set o n         -> (,h) <$> writeCell e o (fromIntegral n)
          MulUpdate s d n -> (,h) <$> mulUpdate e s d n
          MulSet s d n    -> (,h) <$> mulSet e s d n

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
  Width8  -> void $ interpretIO m p (Proxy @Word8)
  Width16 -> void $ interpretIO m p (Proxy @Word16)
  Width32 -> void $ interpretIO m p (Proxy @Word32)
  Width64 -> void $ interpretIO m p (Proxy @Word64)
{-# INLINE runInterpretIO #-}
