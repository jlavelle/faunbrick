module FaunBrick.Interpreter where

import Prelude hiding (read)

import FaunBrick.AST (Program, FaunBrick(..), Instruction(..))

data MachineF i o r
  = Continue r
  | AwaitF (i -> r)
  | YieldF o r
  | DoneF

data Machine i o
  = Await (i -> Machine i o)
  | Yield o (Machine i o)
  | Done

data State a = State
  { _pointer :: !Int
  , _memory  :: !a
  , _program :: !Program
  }

data Memory a i o = Memory
  { _read  :: a -> Int -> o
  , _write :: a -> Int -> i -> a
  }

iterMachine :: (State a -> MachineF i o (State a)) -> State a -> Machine i o
iterMachine f = loop
  where
    loop s = case f s of
      Continue r -> loop r
      AwaitF g   -> Await $ loop . g
      YieldF o r -> Yield o $ loop r
      DoneF      -> Done

type Memory' f a = Memory (f a) a a
type Step f a = MachineF a a (State (f a))

step :: Integral a => Memory' f a -> State (f a) -> Step f a
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
