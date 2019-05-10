module FaunBrick.Interpreter.Types where

import Data.ByteString.Internal (c2w, w2c)
import Data.Functor ((<&>))
import Data.IORef (readIORef, writeIORef, modifyIORef', IORef)
import Data.Maybe (maybe)
import Data.Primitive.Types (Prim)
import Data.Tagged (Tagged(..))
import Data.Vector.Primitive.Mutable (IOVector)
import Data.Word (Word8, Word16, Word32, Word64)
import qualified Data.ByteString.Builder as BS
import qualified Data.Vector.Generic.Mutable as MV

import FaunBrick.Common.Types (Error)

class Memory e m a | e -> a where
  readCell   :: e -> Int -> m a
  writeCell  :: e -> Int -> a -> m ()
  modifyCell :: e -> Int -> (a -> a) -> m ()

instance Prim a => Memory (IOVector a) IO a where
  readCell v i = MV.unsafeRead v i
  {-# INLINE readCell #-}
  writeCell v i a = MV.unsafeWrite v i a
  {-# INLINE writeCell #-}
  modifyCell v i f = MV.unsafeModify v f i
  {-# INLINE modifyCell #-}

class PureMemory e a | e -> a where
  readCellP   :: e -> Int -> Either Error a
  writeCellP  :: e -> Int -> a -> Either Error e
  modifyCellP :: e -> Int -> (a -> a) -> Either Error e

instance PureMemory e a => Memory (Tagged a (IORef e)) IO a where
  readCell (Tagged ref) i     = readIORef ref <&> \e -> unsafeRight $ readCellP e i
  writeCell (Tagged ref) i a  = modifyIORef' ref $ \e -> unsafeRight $ writeCellP e i a
  modifyCell (Tagged ref) i f = modifyIORef' ref $ \e -> unsafeRight $ modifyCellP e i f

-- TODO: handle this properly
unsafeRight :: Show e => Either e a -> a
unsafeRight (Right a) = a
unsafeRight (Left e) = error $ "Memory Error: " <> show e

class Handle e m a | e -> a where
  input  :: e -> m (Maybe a)
  output :: e -> a -> m ()

class PureHandle e a | e -> a where
  inputP  :: e -> (Maybe a, e)
  outputP :: e -> a -> e

instance PureHandle e a => Handle (Tagged a (IORef e)) IO a where
  input (Tagged ref) = do
    e <- readIORef ref
    let (ma, e') = inputP e
    writeIORef ref e'
    pure ma
  output (Tagged ref) a = modifyIORef' ref $ \e -> outputP e a

class Packable a where
  pack     :: a -> BS.Builder
  fromChar :: Char -> a
  default fromChar :: Enum a => Char -> a
  fromChar = toEnum . fromEnum

instance Packable Word8 where
  pack = BS.word8
  fromChar = charToWord

instance Packable Word16 where
  pack a = maybe (BS.word16LE a) pack $ truncatable (maxBound @Word8) a

instance Packable Word32 where
  pack a = maybe (BS.word32LE a) pack $ truncatable (maxBound @Word16) a

instance Packable Word64 where
  pack a = maybe (BS.word64LE a) pack $ truncatable (maxBound @Word32) a

truncatable :: (Integral a, Integral b) => a -> b -> Maybe a
truncatable a b = if b <= fromIntegral a then Just (fromIntegral b) else Nothing

-- TODO: Don't use internal Bytestring functions
wordToChar :: Word8 -> Char
wordToChar = w2c

charToWord :: Char -> Word8
charToWord = c2w
