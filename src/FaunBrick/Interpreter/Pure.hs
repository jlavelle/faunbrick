module FaunBrick.Interpreter.Pure (
  interpret, runFileWith, Faun(..), mkDefaultFaun
) where

import Control.Monad.Except (MonadError, ExceptT, liftEither, throwError, runExceptT)
import Data.Functor.Identity (Identity(..))
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T

import FaunBrick.MonadFaun (MonadFaun(..))
import qualified FaunBrick.Interpret as Interpret
import FaunBrick.AST (FaunBrick)
import FaunBrick.Parser (parseFile)

-- run a file with the given lazy Text input
runFileWith :: Integral a => Text -> FilePath -> IO (Either Error (Faun a))
runFileWith t p = interpret (mkDefaultFaun t) <$> parseFile p

interpret :: Integral a => Faun a -> FaunBrick -> Either Error (Faun a)
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

data Faun a = Faun
  { faunTape :: Tape a
  , faunOut  :: Text
  , faunIn   :: Text
  } deriving Show

instance Integral a => MonadFaun (Faun a) Pure where
  type Pointer (Faun a) = Integer
  type Cell (Faun a) = a

  readCurrentCell (Faun t _ _) = pure $ readFocus t

  modifyPointer e@(Faun t _ _) f = do
    t' <- liftEither (diffMove f t)
    pure $ e { faunTape = t' }

  writeCurrentCell e@(Faun t _ _) c = pure $ e { faunTape = modifyFocus (const c) t }

  input e@(Faun t _ ih) = case T.uncons ih of
    Just (c, ih') -> pure $ e { faunTape = modifyFocus (const $ upcast c) t, faunIn = ih' }
    Nothing       -> throwError NoInput

  output e@(Faun t oh _) = pure $ e { faunOut = T.snoc oh $ downcast $ readFocus t }

data Tape a = Tape [a] a [a] deriving Show

downcast :: (Integral a, Enum b) => a -> b
downcast = toEnum . fromEnum

upcast :: (Enum a, Integral b) => a -> b
upcast = toEnum . fromEnum

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

diffMove :: Integral a => (Integer -> Integer) -> Tape a -> Either Error (Tape a)
diffMove f t =
  let x = fromIntegral $ readFocus t
      y = f x
  in case compare y x of
    GT -> rightN (y - x) t
    LT -> leftN (x - y) t
    _  -> Right t

mkDefaultFaun :: Integral a => Text -> Faun a
mkDefaultFaun i = Faun
  { faunTape = Tape [] 0 $ replicate 29999 0
  , faunOut  = mempty
  , faunIn   = i
  }
