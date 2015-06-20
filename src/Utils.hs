module Utils
    ( getCurrentMonotonicTime
    , newMVar
    , modifyMVar
    , modifyMVar_
    , readMVar
    ) where

import System.Clock
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Concurrent.MVar (MVar)
import qualified Control.Concurrent.MVar as M (newMVar, takeMVar, putMVar, readMVar)

getCurrentMonotonicTime :: MonadIO m
                        => m Integer
getCurrentMonotonicTime = liftIO $ timeSpecAsNanoSecs <$> getTime Monotonic

modifyMVar_ :: (MonadCatch m, MonadMask m, MonadIO m)
            => MVar a
            -> (a -> m a)
            -> m ()
modifyMVar_ m io =
  mask $ \restore -> do
    a  <- liftIO $ M.takeMVar m
    a' <- restore (io a) `onException` liftIO (M.putMVar m a)
    liftIO $ M.putMVar m a'

modifyMVar :: (MonadCatch m, MonadMask m, MonadIO m)
           => MVar a
           -> (a -> m (a, b))
           -> m b
modifyMVar m io =
  mask $ \restore -> do
    a  <- liftIO $ M.takeMVar m
    (a', b) <- restore (io a) `onException` liftIO (M.putMVar m a)
    liftIO $ M.putMVar m a'
    return b

readMVar :: MonadIO m
         => MVar a
         -> m a
readMVar = liftIO . M.readMVar

newMVar :: MonadIO m
        => a
        -> m (MVar a)
newMVar = liftIO . M.newMVar

