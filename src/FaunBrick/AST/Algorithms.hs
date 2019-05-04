{-# LANGUAGE QuasiQuotes #-}

module FaunBrick.AST.Algorithms where

import Prelude hiding (replicate)

import Control.Monad (when)
import Control.Monad.State
import Control.Monad.Writer (WriterT, MonadWriter, tell, runWriterT, censor)
import Control.Monad.Except
import Data.Text (Text)
import qualified Data.Text.Lazy as LT
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as B
import Data.Semigroup (stimesMonoid)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Foldable (fold, minimumBy, traverse_)
import Data.Functor.Foldable (cata)
import Data.Ord (comparing)
import Data.Functor ((<&>))
import TextShow
import Data.Word (Word8)

import FaunBrick.AST hiding (Offset)
import FaunBrick.AST.Util (replicate, single)
import FaunBrick.AST.TH (fb)

newtype Offset = Offset { getOffset :: Int }
  deriving (Show, Eq, Ord, Num, Enum)

newtype Value  = Value { getValue :: Word8 }
   deriving (Show, Eq, Ord, Num)

data Var = Var
  { varName   :: Text
  , varOffset :: Offset
  , varValue  :: Value
  } deriving (Eq, Ord, Show)

data DynVar = DynVar
  { dynVarName   :: Text
  , dynVarOffset :: Offset
  } deriving (Eq, Ord, Show)

type AnyVar = Either DynVar Var

type VarMap = Map Offset AnyVar

data AlgState = AlgState
  { offset    :: Offset
  , varMap    :: VarMap
  , protected :: Set Offset
  } deriving Show

data AlgError
  = VarAlreadyDefined
  | UnknownVar
  | ProtectedWrite
  deriving Show

newtype AlgM a = AlgM { runAlgM :: StateT AlgState (WriterT Program (Except AlgError)) a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadWriter Program
           , MonadState AlgState
           , MonadError AlgError
           )

type AlgCtx m = (MonadWriter Program m, MonadState AlgState m, MonadError AlgError m)

emptyAlgState :: AlgState
emptyAlgState = AlgState 0 M.empty S.empty

evalAlgM :: AlgState -> AlgM a -> Either AlgError (a, AlgState, Program)
evalAlgM s = fmap flat . runExcept . runWriterT . flip runStateT s . runAlgM
  where
    flat ((a, b), c) = (a, b, c)

brickFaun :: Program -> LT.Text
brickFaun = B.toLazyText . cata go
  where
    go :: FaunBrickF Instruction Builder -> Builder
    go HaltF = mempty
    go (InstrF i r) = toBF i <> r
    go (LoopF bs r) = B.singleton '[' <> bs <> B.singleton ']' <> r
    go (IfF _ _) = error "Algorithms: brickFaun does not support If constructs."

    toBF :: Instruction -> Builder
    toBF (Update 0 n) = stimesMonoid (abs n) $ B.singleton $ gtlt0 n '+' '-'
    toBF (Jump n)     = stimesMonoid (abs n) $ B.singleton $ gtlt0 n '>' '<'
    toBF (Output 0)   = B.singleton '.'
    toBF (Input 0)    = B.singleton '-'
    toBF i = error $ "Algorithms: Unsupported operation " <> show i <> " passed to brickFaun."

gtlt0 :: (Num a, Ord a) => a -> b -> b -> b
gtlt0 a g l | a >= 0 = g
            | otherwise = l

setEq :: AlgCtx m => Var -> Var -> m ()
setEq x y = do
  clearVar x
  withTemp $ \t -> do
    jumpTo y
    copyN [x, t] y
    jumpTo t
    copyN [y] t
  modifyVarState x (\(Var t o _) -> Var t o (varValue y))

setPlusEq :: AlgCtx m => Var -> Var -> m ()
setPlusEq x y = do
  withTemp $ \t -> do
    jumpTo y
    copyN [x, t] y
    jumpTo t
    copyN [y] t
  modifyVarState x (\(Var t o v) -> Var t o (v + varValue y))

setMinusEq :: AlgCtx m => Var -> Var -> m ()
setMinusEq x y = do
  withTemp $ \t -> do
    jumpTo y
    loop $ decVar x *> incVar t *> decVar y
    jumpTo t
    copyN [y] t
  modifyVarState x (\(Var t o v) -> Var t o (v - varValue y))

setTimesEq :: AlgCtx m => Var -> Var -> m ()
setTimesEq x y = do
  withTemp $ \t0 -> do
    withTemp $ \t1 -> do
      jumpTo x
      copyN [t1] x
      jumpTo t1
      loop $ do
        jumpTo y
        copyN [x, t0] y
        jumpTo t0
        copyN [y] t0
        decVar t1
  modifyVarState x (\(Var t o v) -> Var t o (v * varValue y))

copyN :: AlgCtx m => [Var] -> Var -> m ()
copyN xs tar = loop $ traverse_ incVar xs *> decVar tar

incVar :: AlgCtx m => Var -> m ()
incVar var = do
  jumpTo var
  tell $ primPlus 1
  modifyVarState var (\(Var t o v) -> Var t o (v + 1))

decVar :: AlgCtx m => Var -> m ()
decVar var = do
  jumpTo var
  tell $ primMinus 1
  modifyVarState var (\(Var t o v) -> Var t o (v - 1))

outputVar :: AlgCtx m => Var -> m ()
outputVar v = jumpTo v *> tell primOutput

jumpTo :: AlgCtx m => Var -> m ()
jumpTo v = jumpI (varOffset v)

modifyVarState :: AlgCtx m => Var -> (Var -> Var) -> m ()
modifyVarState v f = do
  whenM (not <$> varExists (Right v)) $ throwError UnknownVar
  modify (\s -> s { varMap = M.adjust (fmap f) (varOffset v) (varMap s) })

loop :: AlgCtx m => m a -> m a
loop = censor primLoop

withTemp :: AlgCtx m => (Var -> m a) -> m a
withTemp f = do
  o <- fresh
  t <- intro' ("temp" <> showt (getOffset o)) 0 o
  r <- f t
  delete t
  pure r

intro :: AlgCtx m => Text -> Value -> m Var
intro t v = fresh >>= intro' t v

intro' :: AlgCtx m => Text -> Value -> Offset -> m Var
intro' t v o = do
  let var = Var t o v
  whenM (varExists (Right var)) $ throwError VarAlreadyDefined
  modify (\s -> s { varMap = M.insert o (Right var) (varMap s) })
  when (v /= 0) $ setVal var
  pure var

introDyn :: AlgCtx m => Text -> m DynVar
introDyn t = fresh >>= introDyn' t

introDyn' :: AlgCtx m => Text -> Offset -> m DynVar
introDyn' t o = do
  let dv = DynVar t o
  whenM (varExists (Left dv)) $ throwError VarAlreadyDefined
  modify (\s -> s { varMap = M.insert o (Left dv) (varMap s) })
  pure dv

delete :: AlgCtx m => Var -> m ()
delete v = do
  clearVar v
  modify (\s -> s { varMap = M.delete (varOffset v) (varMap s) })

varExists :: AlgCtx m => AnyVar -> m Bool
varExists x = gets varMap <&> (\m -> M.member (anyVarOffset x) m || x `elem` M.elems m)

anyVarOffset :: AnyVar -> Offset
anyVarOffset = either dynVarOffset varOffset

setVal :: AlgCtx m => Var -> m ()
setVal x = do
  st <- get
  let p = snd $ minimumBy (comparing fst) $ setValCosts x st
  tell p

-- TODO: Use some heuristic to find more optimal ways to set the value
setValCosts :: Var -> AlgState -> [(Int, Program)]
setValCosts (Var _ _ v) _ =
  dirPlus : dirMinus : fold []
  where
    dirPlus  = let p = primPlus v in (length p, p)
    dirMinus = let p = primMinus (255 - v) in (length p, p)

-- find the next unallocated cell, clear it, and return its index
fresh :: AlgCtx m => m Offset
fresh = do
  m <- gets (nextIndex . varMap)
  jumpI m *> clear
  pure m

jumpI :: AlgCtx m => Offset -> m ()
jumpI i = do
  o <- gets offset
  tell $ primJump (i - o)
  modify (\s -> s { offset = o + (i - o) })

clearVar :: AlgCtx m => Var -> m ()
clearVar v = jumpTo v *> clear

clear :: AlgCtx m => m ()
clear = tell $ primClear

primPlus :: Value -> Program
primPlus v = replicate v (Update 0 1)

primMinus :: Value -> Program
primMinus v = replicate v (Update 0 (-1))

primJump :: Offset -> Program
primJump (Offset n) = replicate (abs n) $ Jump (div n (abs n))

primClear :: Program
primClear = [fb| [-] |]

primLoop :: Program -> Program
primLoop p = Loop p Halt

primOutput :: Program
primOutput = single $ Output 0

primInput :: Program
primInput = single $ Input 0

-- Move n cells to the right where n is the value of the current cell
primMoveByR :: Program
primMoveByR = [fb| [[>+<-]>-] |]

-- Similar to primMoveByR, but moves to the left instead.
primMoveByL :: Program
primMoveByL = [fb| [[<+>-]<-] |]

nextIndex ::  Map Offset a -> Offset
nextIndex m = foldr go 0 [0..]
  where
    go i a = maybe i (const a) $ M.lookup i m

whenM :: Monad m => m Bool -> m () -> m ()
whenM p a = p >>= \r -> when r a

