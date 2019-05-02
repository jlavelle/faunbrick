{-# LANGUAGE UndecidableInstances #-}

module FaunBrick.Interpreter.Types where

import Control.Monad.Except (MonadError(..), liftEither)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Vector.Primitive.Mutable (IOVector)
import qualified Data.Vector.Generic.Mutable as MV
import qualified GHC.IO.Handle as GHC
import Data.Word (Word8)
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as B
import Data.Functor (($>))
import Data.Maybe (fromJust, maybe)
import Data.ByteString.Internal (c2w, w2c)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as M

data Error
  = OutOfBounds
  | NoInput
  deriving Show

class Memory e m where
  type Cell e
  readCell    :: e -> Int -> m (Cell e)
  movePointer :: e -> (Int -> Int) -> m e
  writeCell   :: e -> Int -> Cell e -> m e
  modifyCell  :: e -> Int -> (Cell e -> Cell e) -> m e

data MVecMem = MVecMem !(IOVector Word8) !Int

instance MonadIO m => Memory MVecMem m where
  type Cell MVecMem = Word8
  readCell (MVecMem v i) o = liftIO $ MV.unsafeRead v (i + o)
  {-# INLINE readCell #-}
  movePointer (MVecMem v i) f = pure $ MVecMem v $ f i
  {-# INLINE movePointer #-}
  writeCell e@(MVecMem v i) o a = liftIO $ MV.unsafeWrite v (i + o) a $> e
  {-# INLINE writeCell #-}
  modifyCell e@(MVecMem v i) o f = liftIO $ MV.modify v f (i + o) $> e
  {-# INLINE modifyCell #-}

data IntMapMem = IntMapMem !(IntMap Word8) !Int

instance Applicative m => Memory IntMapMem m where
  type Cell IntMapMem = Word8
  readCell (IntMapMem m p) o = pure $ M.findWithDefault 0 (p + o) m
  {-# INLINE readCell #-}
  movePointer (IntMapMem m p) f = pure $ IntMapMem m $ f p
  {-# INLINE movePointer #-}
  writeCell (IntMapMem m p) o a = pure $ IntMapMem (M.insert (p + o) a m) p
  {-# INLINE writeCell #-}
  modifyCell (IntMapMem m p) o f = pure $ IntMapMem (M.alter go (p + o) m) p
    where
      go = Just . maybe (f 0) f
  {-# INLINE modifyCell #-}

data Tape a = Tape [a] a [a] deriving Show

-- NB: the Tape doesn't perform as well as the IntMap with the offset optimization
instance MonadError Error m => Memory (Tape Word8) m where
  type Cell (Tape Word8) = Word8
  readCell t o = readFocus <$> liftEither (diffMove t (+ o))
  {-# INLINE readCell #-}
  movePointer t = liftEither . diffMove t
  {-# INLINE movePointer #-}
  writeCell t o a = tapeMod t o (const a)
  {-# INLINE writeCell #-}
  modifyCell = tapeMod
  {-# INLINE modifyCell #-}

tapeMod :: (Integral a, MonadError Error m) => Tape a -> Int -> (a -> a) -> m (Tape a)
tapeMod t o f = do
  t1 <- liftEither $ diffMove t (+ o)
  let t2 = modifyFocus f t1
  liftEither $ diffMove t2 (subtract o)

readFocus :: Tape a -> a
readFocus (Tape _ a _) = a
{-# INLINE readFocus #-}

leftN :: Integral n => n -> Tape a -> Either Error (Tape a)
leftN 0 t = Right t
leftN _ (Tape [] _ _) = Left OutOfBounds
leftN n (Tape (l:ls) f rs) = leftN (n - 1) (Tape ls l (f:rs))
{-# INLINE leftN #-}

rightN :: Integral n => n -> Tape a -> Either Error (Tape a)
rightN 0 t = Right t
rightN _ (Tape _ _ []) = Left OutOfBounds
rightN n (Tape ls f (r:rs)) = rightN (n - 1) (Tape (f:ls) r rs)
{-# INLINE rightN #-}

modifyFocus :: (a -> a) -> Tape a -> Tape a
modifyFocus f (Tape ls a rs) = Tape ls (f a) rs
{-# INLINE modifyFocus #-}

diffMove :: Integral a => Tape a -> (Int -> Int) -> Either Error (Tape a)
diffMove t f =
  let x = fromEnum $ readFocus t
      y = f x
  in case compare y x of
    GT -> rightN (y - x) t
    LT -> leftN (x - y) t
    _  -> Right t
{-# INLINE diffMove #-}

class Handle h m where
  type Out h
  output :: h -> Out h -> m h
  input  :: h -> m (Out h, h)

data IOHandle = IOHandle
  { ioHandleIn  :: GHC.Handle
  , ioHandleOut :: GHC.Handle
  }

instance MonadIO m => Handle IOHandle m where
  type Out IOHandle = Word8
  output h@(IOHandle _ o) a = liftIO $ GHC.hPutChar o (wordToChar a) $> h
  {-# INLINE output #-}
  input h@(IOHandle i _) = liftIO $ (,h) . charToWord <$> GHC.hGetChar i
  {-# INLINE input #-}

data TextHandle = TextHandle
  { textHandleIn  :: LT.Text
  , textHandleOut :: B.Builder
  }

instance MonadError Error m => Handle TextHandle m where
  type Out TextHandle = Word8
  output t = pure . thOut t
  {-# INLINE output #-}
  input = maybe (throwError NoInput) pure . thIn
  {-# INLINE input #-}

newtype UnsafeTextHandle = UnsafeTextHandle TextHandle

instance Monad m => Handle UnsafeTextHandle m where
  type Out UnsafeTextHandle = Word8
  output (UnsafeTextHandle t) = pure . UnsafeTextHandle . thOut t
  {-# INLINE output #-}
  input (UnsafeTextHandle t)  = pure $ UnsafeTextHandle <$> fromJust (thIn t)
  {-# INLINE input #-}

thOut :: TextHandle -> Word8 -> TextHandle
thOut (TextHandle i o) a = TextHandle i $ o <> B.singleton (wordToChar a)
{-# INLINE thOut #-}

thIn :: TextHandle -> Maybe (Word8, TextHandle)
thIn (TextHandle i o) = case LT.uncons i of
  Just (a, r) -> Just (charToWord a, TextHandle r o)
  Nothing -> Nothing
{-# INLINE thIn #-}

-- TODO: Don't use internal Bytestring functions
wordToChar :: Word8 -> Char
wordToChar = w2c
{-# INLINE wordToChar #-}

charToWord :: Char -> Word8
charToWord = c2w
{-# INLINE charToWord #-}
