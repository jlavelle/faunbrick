module FaunBrick.Interpreter where

import Prelude hiding (read)

import Control.Monad.ST (runST)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Functor.Foldable (cata)
import Data.Profunctor (Profunctor(..))
import Data.Word (Word8)
import Data.Vector (Vector)
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as MV
import qualified DeferredFolds.Unfoldl as Unfoldl
import qualified Control.Foldl as Foldl
import System.IO (stdin)

import FaunBrick.AST (Program, FaunBrick(..), FaunBrickF(..), Instruction(..))
import FaunBrick.Common.Types (BitWidth(..), EofMode(..))

data MachineF i o r
  = Continue r
  | AwaitF (i -> r)
  | YieldF o r
  | DoneF

data Machine i o
  = Await (i -> Machine i o)
  | Yield o (Machine i o)
  | Done
  deriving Functor

instance Profunctor Machine where
  dimap f g = \case
    Await f'  -> Await $ dimap f g . f' . f
    Yield o m -> Yield (g o) $ dimap f g m
    Done      -> Done

data State a = State
  { _pointer :: !Int
  , _memory  :: !a
  , _program :: !Program
  }

data Memory f a = Memory
  { _read  :: f a -> Int -> a
  , _write :: f a -> Int -> a -> f a
  }

data Annotated
  = BeginLoop Int
  | EndLoop Int
  | BeginIf Int
  | EndIf Int
  | Instruction Instruction
  | EndProgram

toAnnotated :: Program -> Vector Annotated
toAnnotated = Unfoldl.fold Foldl.vector . end . cata go
  where
    go = \case
      HaltF      -> mempty
      InstrF i r -> pure (Instruction i) <> r
      LoopF a b  -> block BeginLoop EndLoop a b
      IfF a b    -> block BeginIf EndIf a b

    block s e a b =
      let l = Unfoldl.fold Foldl.length a
      in pure (s l) <> a <> pure (e l) <> b

    end p = p <> pure EndProgram

iterMachine :: (State a -> MachineF i o (State a)) -> State a -> Machine i o
iterMachine f = loop
  where
    loop s = case f s of
      Continue r -> loop r
      AwaitF g   -> Await $ loop . g
      YieldF o r -> Yield o $ loop r
      DoneF      -> Done

step :: Integral a => Memory f a -> State (f a) -> MachineF a a (State (f a))
step (Memory read' write') (State ptr mem program) = case program of
  If bs rest   -> branch bs rest
  Loop bs rest -> loop bs rest
  Instr i rest -> runInstr i rest
  Halt         -> DoneF
  where
    read o     = read' mem (ptr + o)
    write o    = write' mem (ptr + o)
    modify o f = write o $ f $ read o

    branch bs r | read 0 == 0 = Continue $ State ptr mem r
                | otherwise   = Continue $ State ptr mem (bs <> r)

    loop bs r | read 0 == 0 = Continue $ State ptr mem r
              | otherwise   = Continue $ State ptr mem (bs <> Loop bs r)

    runInstr ins r = case ins of
      Output o        -> YieldF (read o) $ State ptr mem r
      Input o         -> AwaitF \inp -> State ptr (write o inp) r
      Update o n      -> continueWith $ modify o (+ fromIntegral n)
      Set o n         -> continueWith $ write o $ fromIntegral n
      MulSet s d n    -> continueWith $ write d $ read s * fromIntegral n
      MulUpdate s d n -> continueWith $ modify d (+ (read s * fromIntegral n))
      Jump n -> Continue $ State (ptr + n) mem r
      where
        continueWith mem' = Continue $ State ptr mem' r

class HasCodec a b where
  encode :: a -> b
  decode :: b -> a
  size   :: Int

instance HasCodec Word8 ByteString where
  encode = BS.singleton
  decode = BS.head
  size   = 1

interpretIO :: forall a. (MV.Unbox a, Integral a, HasCodec a ByteString) => EofMode -> Program -> IO ()
interpretIO em = go . dimap decode encode . iterMachine (step vecMem) . initialState
  where
    go = \case
      Yield o r -> BS.putStr o *> go r
      Await f   -> BS.hGet stdin (size @a @ByteString) >>= go . f
      Done      -> pure ()
    initialState = State 999 $ UV.replicate 31000 0

    vecMem :: Memory UV.Vector a
    vecMem = Memory r w
      where
        r v p   = v `UV.unsafeIndex` p
        w v p x = runST do
          mv <- UV.unsafeThaw v
          MV.unsafeWrite mv p x
          UV.unsafeFreeze mv

runInterpretIO :: EofMode -> BitWidth -> Program -> IO ()
runInterpretIO m b p = case b of
  Width8  -> interpretIO @Word8 m p
  _ -> error $ show b <> " width not yet implemented."
  -- Width16 -> interpretIO @Word16 m p
  -- Width32 -> interpretIO @Word32 m p
  -- Width64 -> interpretIO @Word64 m p
