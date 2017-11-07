module World (World(..), WorldT(..), runWorldT) where

import Control.Monad.Base           (MonadBase(..))
import Control.Monad.Catch          (MonadThrow)
import Control.Monad.IO.Class       (MonadIO)
import Control.Monad.Reader         (MonadReader(..))
import Control.Monad.State.Strict   (MonadState(..), StateT(..))
import Control.Monad.Trans.Class    (MonadTrans(..))
import Control.Monad.Trans.Control  (ComposeSt, MonadBaseControl(..), MonadTransControl)
import Control.Monad.Trans.Resource (MonadResource)

data World = World

newtype WorldT m a = WorldT { getWorldT :: StateT World m a }
  deriving (Applicative, Functor, Monad, MonadIO, MonadResource, MonadThrow, MonadTrans, MonadTransControl)

instance MonadBase b m => MonadBase b (WorldT m) where
  liftBase = lift . liftBase

instance MonadBaseControl b m => MonadBaseControl b (WorldT m) where
  type StM (WorldT m) a = ComposeSt WorldT m a
  liftBaseWith f = WorldT $ liftBaseWith $ \ g -> f $ g . getWorldT
  restoreM = WorldT . restoreM

instance MonadReader r m => MonadReader r (WorldT m) where
  ask = lift ask
  local f = WorldT . local f . getWorldT
  reader = lift . reader

instance Monad m => MonadState World (WorldT m) where
  get = WorldT get
  put = WorldT . put
  state = WorldT . state

runWorldT :: Monad m => World -> WorldT m a -> m (a, World)
runWorldT world WorldT {..} = runStateT getWorldT world
