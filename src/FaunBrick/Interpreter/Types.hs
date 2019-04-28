{-# LANGUAGE UndecidableInstances #-}

module FaunBrick.Interpreter.Types where

import Control.Monad.Except (MonadError(..), liftEither)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Vector.Primitive.Mutable (IOVector)
import qualified Data.Vector.Generic.Mutable as MV
import qualified GHC.IO.Handle as GHC
import Data.Word (Word8)
import qualified Data.Text.Lazy as LT
import Data.Functor (($>))
import Data.Maybe (fromJust, fromMaybe)
import Data.ByteString.Internal (c2w, w2c)

data Error
  = OutOfBounds
  | NoInput
  deriving Show

class Memory e m where
  type Cell e
  readCell    :: e -> m (Cell e)
  movePointer :: e -> (Int -> Int) -> m e
  writeCell   :: e -> Cell e -> m e

data MVecMem = MVecMem (IOVector Word8) Int

instance MonadIO m => Memory MVecMem m where
  type Cell MVecMem = Word8
  readCell (MVecMem v i) = liftIO $ MV.read v i
  movePointer (MVecMem v i) f = pure $ MVecMem v $ f i
  writeCell e@(MVecMem v i) a = liftIO $ MV.write v i a $> e

data Tape a = Tape [a] a [a] deriving Show

instance MonadError Error m => Memory (Tape Word8) m where
  type Cell (Tape Word8) = Word8
  readCell = pure . readFocus
  movePointer t = liftEither . diffMove t
  writeCell t a = pure $ modifyFocus (const a) t

readFocus :: Tape a -> a
readFocus (Tape _ a _) = a

leftN :: Integral n => n -> Tape a -> Either Error (Tape a)
leftN 0 t = Right t
leftN _ (Tape [] _ _) = Left OutOfBounds
leftN n (Tape (l:ls) f rs) = leftN (n - 1) (Tape ls l (f:rs))

rightN :: Integral n => n -> Tape a -> Either Error (Tape a)
rightN 0 t = Right t
rightN _ (Tape _ _ []) = Left OutOfBounds
rightN n (Tape ls f (r:rs)) = rightN (n - 1) (Tape (f:ls) r rs)

modifyFocus :: (a -> a) -> Tape a -> Tape a
modifyFocus f (Tape ls a rs) = Tape ls (f a) rs

diffMove :: Integral a => Tape a -> (Int -> Int) -> Either Error (Tape a)
diffMove t f =
  let x = fromEnum $ readFocus t
      y = f x
  in case compare y x of
    GT -> rightN (y - x) t
    LT -> leftN (x - y) t
    _  -> Right t

class Handle h m where
  type Out h
  output :: h -> Out h -> m h
  input  :: h -> m (Out h, h)

--                       out        in
data IOHandle = IOHandle GHC.Handle GHC.Handle

instance MonadIO m => Handle IOHandle m where
  type Out IOHandle = Word8
  output h@(IOHandle o _) a = liftIO $ GHC.hPutChar o (wordToChar a) $> h
  input  h@(IOHandle _ i) = liftIO $ (,h) . charToWord <$> GHC.hGetChar i

data TextHandle = TextHandle LT.Text LT.Text

instance MonadError Error m => Handle TextHandle m where
  type Out TextHandle = Word8
  output t = pure . textHandleOut t
  input = fromMaybe (throwError NoInput) . fmap pure . textHandleIn

newtype UnsafeTextHandle = UnsafeTextHandle TextHandle

instance Monad m => Handle UnsafeTextHandle m where
  type Out UnsafeTextHandle = Word8
  output (UnsafeTextHandle t) = pure . UnsafeTextHandle . textHandleOut t
  input (UnsafeTextHandle t)  = pure $ UnsafeTextHandle <$> fromJust (textHandleIn t)

textHandleOut :: TextHandle -> Word8 -> TextHandle
textHandleOut (TextHandle i o) a = TextHandle (LT.snoc o $ wordToChar a) i

textHandleIn :: TextHandle -> Maybe (Word8, TextHandle)
textHandleIn (TextHandle i o) = case LT.uncons i of
  Just (a, r) -> Just (charToWord a, TextHandle o r)
  Nothing -> Nothing

-- TODO: Don't use internal Bytestring functions
wordToChar :: Word8 -> Char
wordToChar = w2c

charToWord :: Char -> Word8
charToWord = c2w
