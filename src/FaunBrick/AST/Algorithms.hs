{-# LANGUAGE QuasiQuotes #-}

module FaunBrick.AST.Algorithms where

import Prelude hiding (replicate)

import Control.Monad (when, replicateM, replicateM_, void)
import Control.Monad.Except (Except, MonadError, throwError, runExcept)
import Control.Monad.State (StateT, MonadState, runStateT, gets, modify)
import Control.Monad.Writer (WriterT, MonadWriter, tell, runWriterT, censor)
import Data.Foldable (minimumBy)
import Data.Functor ((<&>), ($>))
import Data.Functor.Foldable (cata)
import Data.Map (Map)
import Data.Monoid (Sum(..))
import Data.Ord (comparing)
import Data.Semigroup (stimesMonoid)
import Data.Set (Set)
import Data.Text (Text)
import Data.Text.Lazy.Builder (Builder)
import Data.Word (Word8)
import TextShow (showt)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as B
import qualified Data.Text.Lazy.IO as LT

import FaunBrick.AST hiding (Offset)
import FaunBrick.AST.Optimize (optimize)
import FaunBrick.AST.TH (fb)
import FaunBrick.AST.Util (replicate, single)
import FaunBrick.Common.Types (EofMode(..), BitWidth(..))
import FaunBrick.Interpreter (runInterpretIO)
import FaunBrick.Interpreter.Types (charToWord)

newtype Offset = Offset { getOffset :: Int }
  deriving (Show, Eq, Ord, Num, Enum)

newtype Value  = Value { getValue :: Word8 }
   deriving (Show, Eq, Ord, Num, Real, Enum, Integral)

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
  | UnknownVar AnyVar
  | ProtectedWrite
  | InternalError String
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

anyVarOffset :: AnyVar -> Offset
anyVarOffset = either dynVarOffset varOffset

varExists :: AlgCtx m => AnyVar -> m Bool
varExists x = gets varMap <&> (\m -> M.member (anyVarOffset x) m || x `elem` M.elems m)

guardUnknownVar :: AlgCtx m => AnyVar -> m ()
guardUnknownVar x = whenM (not <$> varExists x) $ throwError $ UnknownVar x

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
    runInterpretIO NoChange Width8 (optimize p)

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

allocString :: AlgCtx m => String -> m [Var]
allocString = allocList . fmap charToWord

outputString :: AlgCtx m => String -> m ()
outputString = void . traverseAllocList outputVar . fmap charToWord

allocList :: AlgCtx m => [Word8] -> m [Var]
allocList = traverseAllocList pure

traverseAllocList :: AlgCtx m => (Var -> m a) -> [Word8] -> m [a]
traverseAllocList f xs = traverseAllocList' (optimalDiv $ fromEnum <$> xs) f xs

-- TODO: Find a span of contiguous memory in which to allocate the list
-- Allows you to manually pass in the divisor
traverseAllocList' :: AlgCtx m => Int -> (Var -> m a) -> [Word8] -> m [a]
traverseAllocList' q f xs = do
  x <- mkTemp (fromIntegral q)
  let (mfs, as) = unzip $ fmap (flip quotRem q . fromEnum) xs
  ts  <- zip mfs <$> tempsN (length mfs)
  ts' <- plusTimesN ts x
  traverse (\(a, t) -> replicateM_ a (incVar t) *> f t)  (zip as ts')

setEq :: AlgCtx m => Var -> Var -> m Var
setEq x y = do
  clearVar x
  withTemp_ $ \t -> do
    _ <- plusN [x, t] y
    plusN [y] t
  modifyVarState x (\(Var t o _) -> Var t o (varValue y))

setPlusEq :: AlgCtx m => Var -> Var -> m Var
setPlusEq x y = do
  withTemp_ $ \t -> do
    _ <- plusN [x, t] y
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
plusN xs = plusTimesN $ zip (repeat 1) xs

-- takes a list of variables paired with a multiplicative factor k and sets each one to
-- xn += k * t, clearing t in the process.
plusTimesN :: AlgCtx m => [(Int, Var)] -> Var -> m [Var]
plusTimesN xs t = jumpTo t *> loop go
  where
    go = traverse (\(i, v) -> replicateM_ i (incVar v) $> v) xs <* decVar t

setVal :: AlgCtx m => Var -> m ()
setVal var@(Var _ _ v)
  | v < 15   = void $ addToVar var (fromIntegral v) *> jumpTo var
  | v >= 221 = void $ addToVar var (fromIntegral v - 256) *> jumpTo var
  | otherwise =
      let (f1, f2, r) = mulAddFactors v
      in void $ basicMulAdd var f1 f2 *> addToVar var r *> jumpTo var

-- adds a * b to the provided var
basicMulAdd :: AlgCtx m => Var -> Int -> Int -> m ()
basicMulAdd v a b = withTempVal (fromIntegral a) $ \x -> do
  loop $ do
    _ <- addToVar v b
    decVar x
  pure ()

mulAddFactors :: Value -> (Int, Int, Int)
mulAddFactors n =
  let n' = round (fromIntegral n / (5 :: Double)) * 5
      r  = fromIntegral n - n'
      fs = go (n', 2)
      (f1, f2) = (product $ init fs, last fs)
  in (max f1 f2, min f1 f2, r)
  where
    go (a, b) = case quotRem a b of
      (x, y) | y == 0    -> b : go (x, b)
             | x == 0    -> []
             | otherwise -> go (a, b + 1)

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

addToVar :: AlgCtx m => Var -> Int -> m Var
addToVar v n | n > 0 = replicateM_ n (incVar v) $> v
             | n < 0 = replicateM_ (abs n) (decVar v) $> v
             | otherwise = pure v

outputVar :: AlgCtx m => Var -> m Var
outputVar v = jumpTo v *> tell primOutput $> v

modifyVarState :: AlgCtx m => Var -> (Var -> Var) -> m Var
modifyVarState v f = do
  guardUnknownVar $ Right v
  modify (\s -> s { varMap = M.adjust (fmap f) (varOffset v) (varMap s) })
  pure v

-- Use a temporary variable in a computation and deallocate it afterwards.
withTemp :: AlgCtx m => (Var -> m a) -> m a
withTemp = withTempVal 0

withTempVal :: AlgCtx m => Value -> (Var -> m a) -> m a
withTempVal v f = do
  t <- mkTemp v
  r <- f t
  deleteVar t
  pure r

withTemp_ :: AlgCtx m => (Var -> m a) -> m ()
withTemp_ = void . withTemp

-- Make a temporary variable.  It must be deallocated manually.
mkTemp :: AlgCtx m => Value -> m Var
mkTemp v = do
  o <- fresh
  intro' ("____temp" <> showt (getOffset o)) v o

tempsN :: AlgCtx m => Int -> m [Var]
tempsN i = replicateM i $ mkTemp 0 >>= clearVar

intro :: AlgCtx m => Text -> Value -> m Var
intro t v = fresh >>= intro' t v

intro' :: AlgCtx m => Text -> Value -> Offset -> m Var
intro' t v o = do
  let var = Var t o v
  whenM (varExists $ Right var) $ throwError VarAlreadyDefined
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
  _ <- clearAnyVar v
  modify (\s -> s { varMap = M.delete (anyVarOffset v) (varMap s) })

-- Jump to the next unallocated cell, clear it, and return its offset
fresh :: AlgCtx m => m Offset
fresh = do
  m <- basicFresh
  jumpI m *> clear
  pure m

-- Find the next unallocated cell in the VarMap
basicFresh :: AlgCtx m => m Offset
basicFresh = gets (nextIndex . varMap)

jumpTo :: AlgCtx m => Var -> m ()
jumpTo = jumpToAnyVar . Right

jumpToAnyVar :: AlgCtx m => AnyVar -> m ()
jumpToAnyVar v = jumpI (anyVarOffset v)

jumpI :: AlgCtx m => Offset -> m ()
jumpI i = do
  o <- gets offset
  tell $ primJump (i - o)
  modify (\s -> s { offset = o + (i - o) })

clearVar :: AlgCtx m => Var -> m Var
clearVar v = do
  _ <- clearAnyVar $ Right v
  modifyVarState v (\(Var t o _) -> Var t o 0)

clearAnyVar :: AlgCtx m => AnyVar -> m AnyVar
clearAnyVar v = jumpToAnyVar v *> clear $> v

clear :: AlgCtx m => m ()
clear = tell primClear

loop :: AlgCtx m => m a -> m a
loop = censor primLoop

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

-- allocates all available input into adjacent cells
primConsumeInput :: Program
primConsumeInput = [fb| ,[>,] |]

nextIndex ::  Map Offset a -> Offset
nextIndex m = foldr go 0 [0..]
  where
    go i a = maybe i (const a) $ M.lookup i m

whenM :: Monad m => m Bool -> m () -> m ()
whenM p a = p >>= \r -> when r a

-- Note: very partial
optimalDiv :: Integral b => [Int] -> b
optimalDiv xs = fromIntegral $ fst $ minimumBy (comparing snd) candidates
  where
    candidates = [ (a, qrSum a xs) | a <- [1..maximum xs]]
    qrSum q = foldMap $ Sum . uncurry (+) . flip quotRem q
