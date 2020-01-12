module FaunBrick.Interpreter where

import Prelude hiding (read)

import Control.Monad.ST (runST)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Functor.Foldable (cata)
import Data.Profunctor (Profunctor(..))
import Data.Word (Word8)
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as MV
import qualified DeferredFolds.Unfoldl as Unfoldl
import qualified Control.Foldl as Foldl
import System.IO (stdin)

import FaunBrick.AST (Program, FaunBrickF(..), Instruction(..))
import FaunBrick.Common.Types (BitWidth(..), EofMode(..))

data MachineF i o r
  = Continue r
  | AwaitF (i -> r)
  | YieldF o r
  | DoneF
  deriving Functor

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
  { _memPointer :: !Int
  , _insPointer :: !Int
  , _memory     :: !a
  , _program    :: !(Vector ProgAnn)
  }

data Memory f a = Memory
  { _read  :: f a -> Int -> a
  , _write :: f a -> Int -> a -> f a
  }

data BlockType = BlLoop | BlIf
  deriving Show

data ProgAnn
  = BeginBlock BlockType Int
  | EndBlock BlockType Int
  | Instruction Instruction
  | EndProgram
  deriving Show

-- TODO: Track the length of each block via the cata rather than
-- using `length`
toAnnotated :: Program -> Vector ProgAnn
toAnnotated = V.imap jumps . Unfoldl.fold Foldl.vector . end . cata go
  where
    go = \case
      HaltF      -> mempty
      InstrF i r -> pure (Instruction i) <> r
      LoopF a b  -> block BlLoop a b
      IfF a b    -> block BlIf a b

    block t a b =
         pure (BeginBlock t $ length a) <> a
      <> pure (EndBlock t   $ length a) <> b

    end p = p <> pure EndProgram

    jumps i = \case
      BeginBlock t l -> BeginBlock t $ i + l + 1
      EndBlock t l   -> EndBlock t   $ i - l
      x -> x

iterMachine :: (State a -> MachineF i o (State a)) -> State a -> Machine i o
iterMachine f = loop
  where
    loop s = case f s of
      Continue r -> loop r
      AwaitF g   -> Await $ loop . g
      YieldF o r -> Yield o $ loop r
      DoneF      -> Done

step :: Integral a => Memory f a -> State (f a) -> MachineF a a (State (f a))
step (Memory read' write') s@(State mptr iptr mem p) = case p `V.unsafeIndex` iptr of
  BeginBlock _ i | read 0 == 0 -> Continue $ setInsPtr i s
                 | otherwise   -> Continue $ setInsPtr (iptr + 1) s
  EndBlock t i -> case t of
    BlLoop | read 0 == 0 -> Continue $ setInsPtr (iptr + 1) s
           | otherwise   -> Continue $ setInsPtr i s
    BlIf -> Continue $ setInsPtr (iptr + 1) s
  Instruction i -> runInstr i
  EndProgram    -> DoneF
  where
    setInsPtr n x = x { _insPointer = n }

    read o     = read' mem $ mptr + o
    write o    = write' mem $ mptr + o
    modify o f = write o $ f $ read o

    runInstr ins = case ins of
      Output o        -> YieldF (read o) $ s { _insPointer = iptr + 1 }
      Input o         -> AwaitF \inp -> s { _memory = write o inp, _insPointer = iptr + 1 }
      Update o n      -> continueWith $ modify o (+ fromIntegral n)
      Set o n         -> continueWith $ write o $ fromIntegral n
      MulSet x d n    -> continueWith $ write d $ read x * fromIntegral n
      MulUpdate x d n -> continueWith $ modify d (+ (read x * fromIntegral n))
      Jump n -> Continue $ s { _memPointer = _memPointer s + n, _insPointer = iptr + 1 }
      where
        continueWith mem' = Continue $ s { _memory = mem', _insPointer = iptr + 1 }

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
    initialState = State 999 0 (UV.replicate 31000 0) . toAnnotated

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
