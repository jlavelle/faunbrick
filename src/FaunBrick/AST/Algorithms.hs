{-# LANGUAGE QuasiQuotes #-}

module FaunBrick.AST.Algorithms where

import Prelude hiding (replicate)

import Control.Monad (when)
import Control.Monad.State
import Control.Monad.Writer (WriterT, MonadWriter, tell, runWriterT, censor)
import Control.Monad.Except
import Data.Text (Text)
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT
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
import Data.Functor ((<&>), ($>))
import TextShow
import Data.Word (Word8)

import FaunBrick.AST hiding (Offset)
import FaunBrick.AST.Util (replicate, single)
import FaunBrick.AST.TH (fb)
import FaunBrick.Common.Types (EofMode(..), BitWidth(..))
import FaunBrick.Interpreter (runInterpretIO)

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

newtype AlgM a = AlgM { getAlgM :: StateT AlgState (WriterT Program (Except AlgError)) a }
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

runAlgM :: AlgState -> AlgM a -> Either AlgError (a, AlgState, Program)
runAlgM s = fmap flat . runExcept . runWriterT . flip runStateT s . getAlgM
  where
    flat ((a, b), c) = (a, b, c)

evalAlgM :: AlgState -> AlgM a -> Either AlgError Program
evalAlgM s = fmap f . runAlgM s
  where f (_,_,x) = x

interpretAlgM :: AlgM a -> IO ()
interpretAlgM a = case runAlgM emptyAlgState a of
  Left e -> error $ "AlgM Error: " <> show e
  Right (_, s, p) -> do
    putStrLn "Program:"
    let bf = brickFaun p
    LT.putStrLn bf
    putStrLn "AlgState:"
    print s
    putStrLn "Result:"
    runInterpretIO NoChange Width8 p

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

helloWorld :: AlgCtx m => m ()
helloWorld = traverse_ outputVar =<< allocString "Hello, World!\n"

allocString :: AlgCtx m => String -> m [Var]
allocString str = withTemp $ \t10 -> do
  replicateM_ 10 $ incVar t10
  let (mfs, as) = unzip $ fmap (flip divMod 10 . fromEnum) str
  ts  <- zip mfs <$> tempsN (length mfs)
  ts' <- plusTimesN ts t10
  forM_ (zip as ts') $ \(a, t) -> replicateM_ a (incVar t)
  pure ts'

setEq :: AlgCtx m => Var -> Var -> m Var
setEq x y = do
  clearVar x
  withTemp_ $ \t -> do
    plusN [x, t] y
    plusN [y] t
  modifyVarState x (\(Var t o _) -> Var t o (varValue y))

setPlusEq :: AlgCtx m => Var -> Var -> m Var
setPlusEq x y = do
  withTemp_ $ \t -> do
    plusN [x, t] y
    plusN [y] t
  modifyVarState x (\(Var t o v) -> Var t o (v + varValue y))

setMinusEq :: AlgCtx m => Var -> Var -> m Var
setMinusEq x y = do
  withTemp_ $ \t -> do
    jumpTo y
    loop $ decVar x *> incVar t *> decVar y
    plusN [y] t
  modifyVarState x (\(Var t o v) -> Var t o (v - varValue y))

setTimesEq :: AlgCtx m => Var -> Var -> m Var
setTimesEq x y = do
  withTemp_ $ \t0 ->
    withTemp_ $ \t1 -> do
      plusN [t1] x
      jumpTo t1
      loop $ do
        plusN [x, t0] y
        plusN [y] t0
        decVar t1
  modifyVarState x (\(Var t o v) -> Var t o (v * varValue y))

-- add t to each var in xs, clearing t in the process.
plusN :: AlgCtx m => [Var] -> Var -> m [Var]
plusN xs tar = plusTimesN (zip (repeat 1) xs) tar

-- takes a list of variables paired with a multiplicative factor k and sets each one to
-- xn += k * t, clearing t in the process.
plusTimesN :: AlgCtx m => [(Int, Var)] -> Var -> m [Var]
plusTimesN xs t = jumpTo t *> loop go
  where
    go = traverse (\(i, v) -> replicateM_ i (incVar v) $> v) xs <* decVar t

incVar :: AlgCtx m => Var -> m Var
incVar var = do
  jumpTo var
  tell $ primPlus 1
  modifyVarState var (\(Var t o v) -> Var t o (v + 1))

decVar :: AlgCtx m => Var -> m Var
decVar var = do
  jumpTo var
  tell $ primMinus 1
  modifyVarState var (\(Var t o v) -> Var t o (v - 1))

outputVar :: AlgCtx m => Var -> m Var
outputVar v = jumpTo v *> tell primOutput $> v

jumpTo :: AlgCtx m => Var -> m ()
jumpTo = jumpToAnyVar . Right

jumpToAnyVar :: AlgCtx m => AnyVar -> m ()
jumpToAnyVar v = jumpI (anyVarOffset v)

modifyVarState :: AlgCtx m => Var -> (Var -> Var) -> m Var
modifyVarState v f = do
  whenM (not <$> varExists (Right v)) $ throwError UnknownVar
  modify (\s -> s { varMap = M.adjust (fmap f) (varOffset v) (varMap s) })
  pure v

loop :: AlgCtx m => m a -> m a
loop = censor primLoop

-- Use a temporary variable in a computation and deallocate it afterwards.
withTemp :: AlgCtx m => (Var -> m a) -> m a
withTemp f = do
  t <- mkTemp
  -- TODO protect t
  r <- f t
  -- TODO unprotect t
  deleteVar t
  pure r

withTemp_ :: AlgCtx m => (Var -> m a) -> m ()
withTemp_ = void . withTemp

-- Make a temporary variable.  It must be deallocated manually or with runGC
mkTemp :: AlgCtx m => m Var
mkTemp = do
  o <- fresh
  intro' ("____temp" <> showt (getOffset o)) 0 o

tempsN :: AlgCtx m => Int -> m [Var]
tempsN i = replicateM i mkTemp

-- -- Delete all unprotected temp variables.  Make sure they aren't in use anymore!
-- runGC :: AlgCtx m => m ()
-- runGC = do
--   m <- gets varMap
--   p <- gets protected
--   let ts = filter TODO

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

deleteVar :: AlgCtx m => Var -> m ()
deleteVar = deleteAnyVar . Right

deleteDynVar :: AlgCtx m => DynVar -> m ()
deleteDynVar = deleteAnyVar . Left

deleteAnyVar :: AlgCtx m => AnyVar -> m ()
deleteAnyVar v = do
  clearAnyVar v
  modify (\s -> s { varMap = M.delete (anyVarOffset v) (varMap s) })

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
clearVar = clearAnyVar . Right

clearAnyVar :: AlgCtx m => AnyVar -> m ()
clearAnyVar v = jumpToAnyVar v *> clear

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

