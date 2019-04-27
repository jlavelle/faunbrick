module FaunBrick.Interpreter.Pure where

import Control.Monad.Except (MonadError, ExceptT, liftEither, throwError, runExceptT)
import Data.Functor.Identity (Identity(..))
import Data.Word (Word8)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Data.ByteString.Internal (c2w, w2c)

import FaunBrick.MonadFaun (MonadFaun(..))
import qualified FaunBrick.Interpret as Interpret
import FaunBrick.AST (Brick)
import FaunBrick.Parser (parseFile)

runFileWith :: Text -> FilePath -> IO (Either Error Faun)
runFileWith t p = interpret (defaultFaun t) <$> parseFile p

interpret :: Foldable f => Faun -> f Brick -> Either Error Faun
interpret f = runIdentity . runExceptT . runPure . Interpret.interpret f

newtype Pure a = Pure { runPure :: ExceptT Error Identity a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadError Error
           )

data Error
  = OutOfBounds
  | NoInput
  deriving Show

data Faun = Faun
  { faunTape :: Tape Word8
  , faunOut  :: Text
  , faunIn   :: Text
  } deriving Show

instance MonadFaun Faun Pure where
  type Pointer Faun = Int
  type Cell Faun = Word8

  readCurrentCell (Faun t _ _) = pure $ readFocus t

  modifyPointer e@(Faun t _ _) f = do
    t' <- liftEither (diffMove f t)
    pure $ e { faunTape = t' }

  writeCurrentCell e@(Faun t _ _) c = pure $ e { faunTape = modifyFocus (const c) t }

  input e@(Faun t _ ih) = case T.uncons ih of
    Just (c, ih') -> pure $ e { faunTape = modifyFocus (const $ c2w c) t, faunIn = ih' }
    Nothing       -> throwError NoInput

  output e@(Faun t oh _) = pure $ e { faunOut = T.snoc oh $ w2c $ readFocus t }

data Tape a = Tape ![a] a ![a] deriving Show

readFocus :: Tape a -> a
readFocus (Tape _ a _) = a

leftN :: Int -> Tape a -> Either Error (Tape a)
leftN 0 t = Right t
leftN _ (Tape [] _ _) = Left OutOfBounds
leftN n (Tape (l:ls) f rs) = leftN (n - 1) (Tape ls l (f:rs))

rightN :: Int -> Tape a -> Either Error (Tape a)
rightN 0 t = Right t
rightN _ (Tape _ _ []) = Left OutOfBounds
rightN n (Tape ls f (r:rs)) = rightN (n - 1) (Tape (f:ls) r rs)

modifyFocus :: (a -> a) -> Tape a -> Tape a
modifyFocus f (Tape ls a rs) = Tape ls (f a) rs

diffMove :: (Int -> Int) -> Tape Word8 -> Either Error (Tape Word8)
diffMove f t =
  let x = fromEnum $ readFocus t
      y = f x
  in case compare y x of
    GT -> rightN (y - x) t
    LT -> leftN (x - y) t
    _  -> Right t

defaultFaun :: Text -> Faun
defaultFaun i = Faun
  { faunTape = Tape [] 0 $ replicate 29999 0
  , faunOut  = mempty
  , faunIn   = i
  }
