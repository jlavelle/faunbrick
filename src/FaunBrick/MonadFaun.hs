module FaunBrick.MonadFaun (
  MonadFaun(..),
  modifyCell,
  forward,
  backward,
  add,
  sub
) where

-- Abstract interface for a FaunBrick interpreter
class Monad m => MonadFaun e m where
  type Pointer e
  type Cell e
  readCurrentCell  :: e -> m (Cell e)
  modifyPointer    :: e -> (Pointer e -> Pointer e) -> m e
  writeCurrentCell :: e -> Cell e -> m e
  input            :: e -> m e
  output           :: e -> m e

modifyCell :: MonadFaun e m => e -> (Cell e -> Cell e) -> m e
modifyCell e f = readCurrentCell e >>= writeCurrentCell e . f

forward :: (Num (Pointer e), MonadFaun e m) => e -> m e
forward e = modifyPointer e (+ 1)

backward :: (Num (Pointer e), MonadFaun e m) => e -> m e
backward e = modifyPointer e (subtract 1)

add :: (Num (Cell e), MonadFaun e m) => e -> m e
add e = modifyCell e (+ 1)

sub :: (Num (Cell e), MonadFaun e m) => e -> m e
sub e = modifyCell e (subtract 1)
