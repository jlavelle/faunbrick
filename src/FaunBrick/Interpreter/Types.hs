module FaunBrick.Interpreter.Types where

import Control.Monad.Except (MonadError(..), liftEither)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString.Internal (c2w, w2c)
import Data.Functor (($>))
import Data.IntMap.Strict (IntMap)
import Data.Maybe (maybe)
import Data.Primitive.Types (Prim)
import Data.Vector.Primitive.Mutable (IOVector)
import Data.Word (Word8, Word16, Word32, Word64)
import System.IO.Error (isEOFError, catchIOError)
import qualified Data.ByteString.Builder as BS
import qualified Data.IntMap.Strict as M
import qualified Data.Text.Lazy as LT
import qualified Data.Vector.Generic.Mutable as MV
import qualified GHC.IO.Handle as GHC

data Error = OutOfBounds deriving Show

class Memory e m where
  type Cell e
  readCell    :: e -> Int -> m (Cell e)
  movePointer :: e -> (Int -> Int) -> m e
  writeCell   :: e -> Int -> Cell e -> m e
  modifyCell  :: e -> Int -> (Cell e -> Cell e) -> m e

data MVecMem a = MVecMem !(IOVector a) !Int

instance (Prim a, MonadIO m) => Memory (MVecMem a) m where
  type Cell (MVecMem a) = a
  readCell (MVecMem v i) o = liftIO $ MV.unsafeRead v (i + o)
  movePointer (MVecMem v i) f = pure $ MVecMem v $ f i
  writeCell e@(MVecMem v i) o a = liftIO $ MV.unsafeWrite v (i + o) a $> e
  modifyCell e@(MVecMem v i) o f = liftIO $ MV.modify v f (i + o) $> e

data IntMapMem a = IntMapMem !(IntMap a) !Int

instance (Integral a, Applicative m) => Memory (IntMapMem a) m where
  type Cell (IntMapMem a) = a
  readCell (IntMapMem m p) o = pure $ M.findWithDefault 0 (p + o) m
  movePointer (IntMapMem m p) f = pure $ IntMapMem m $ f p
  writeCell (IntMapMem m p) o a = pure $ IntMapMem (M.insert (p + o) a m) p
  modifyCell (IntMapMem m p) o f = pure $ IntMapMem (M.alter go (p + o) m) p
    where
      go = Just . maybe (f 0) f

data Tape a = Tape [a] a [a] deriving Show

-- NB: the Tape may not perform as well as the IntMap with the offset optimization
instance (Integral a, MonadError Error m) => Memory (Tape a) m where
  type Cell (Tape a) = a
  readCell t o = readFocus <$> liftEither (diffMove t (+ o))
  movePointer t = liftEither . diffMove t
  writeCell t o a = tapeMod t o (const a)
  modifyCell = tapeMod

tapeMod :: (Integral a, MonadError Error m) => Tape a -> Int -> (a -> a) -> m (Tape a)
tapeMod t o f = do
  t1 <- liftEither $ diffMove t (+ o)
  let t2 = modifyFocus f t1
  liftEither $ diffMove t2 (subtract o)

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

class Packable a where
  pack     :: a -> BS.Builder
  fromChar :: Char -> a
  default fromChar :: Enum a => Char -> a
  fromChar = toEnum . fromEnum

instance Packable Word8 where
  pack = BS.word8
  fromChar = charToWord

-- We try to downcast larger Words to Word8 so the resulting ByteString
-- is not padded if possible.  Not sure if this makes sense or not yet,
-- but it works for programs like pi.b
instance Packable Word16 where
  pack a = maybe (BS.word16LE a) pack $ truncatable (maxBound @Word8) a

instance Packable Word32 where
  pack a = maybe (BS.word32LE a) pack $ truncatable (maxBound @Word16) a

instance Packable Word64 where
  pack a = maybe (BS.word64LE a) pack $ truncatable (maxBound @Word32) a

truncatable :: (Integral a, Integral b) => a -> b -> Maybe a
truncatable a b = if b <= fromIntegral a then Just (fromIntegral b) else Nothing

class Packable (Out h) => Handle h m where
  type Out h
  output  :: h -> Out h -> m h
  input   :: h -> m (Maybe (Out h), h)

data IOHandle a = IOHandle
  { ioHandleIn  :: GHC.Handle
  , ioHandleOut :: GHC.Handle
  }

instance (Packable a, MonadIO m) => Handle (IOHandle a) m where
  type Out (IOHandle a) = a
  output h@(IOHandle _ o) a = liftIO $ BS.hPutBuilder o (pack a) $> h
  input h@(IOHandle i _) = liftIO $ do
    c <- catchIOError (Just . fromChar <$> GHC.hGetChar i) handler
    pure (c, h)
    where
      handler e = if isEOFError e then pure Nothing else ioError e

data TextHandle a = TextHandle
  { textHandleIn  :: LT.Text
  , textHandleOut :: BS.Builder
  }

instance (Packable a, Applicative m) => Handle (TextHandle a) m where
  type Out (TextHandle a) = a
  output t = pure . thOut t
  input = pure . thIn

thOut :: Packable a => TextHandle a -> a -> TextHandle a
thOut (TextHandle i o) a = TextHandle i $ o <> pack a

thIn :: Packable a => TextHandle a -> (Maybe a, TextHandle a)
thIn h@(TextHandle i o) = case LT.uncons i of
  Just (a, r) -> (Just $ fromChar a, TextHandle r o)
  Nothing -> (Nothing, h)

-- TODO: Don't use internal Bytestring functions
wordToChar :: Word8 -> Char
wordToChar = w2c

charToWord :: Char -> Word8
charToWord = c2w
