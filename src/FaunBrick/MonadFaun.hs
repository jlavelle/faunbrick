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

modifyCell :: MonadFaun e m => (Cell e -> Cell e) -> e -> m e
modifyCell f e = readCurrentCell e >>= writeCurrentCell e . f

forward :: (Num (Pointer e), MonadFaun e m) => e -> m e
forward e = modifyPointer e (+ 1)

backward :: (Num (Pointer e), MonadFaun e m) => e -> m e
backward e = modifyPointer e (subtract 1)

add :: (Num (Cell e), MonadFaun e m) => e -> m e
add e = modifyCell (+ 1) e

sub :: (Num (Cell e), MonadFaun e m) => e -> m e
sub e = modifyCell (subtract 1) e
