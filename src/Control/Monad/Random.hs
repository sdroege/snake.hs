{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeFamilies, UndecidableInstances #-}

module Control.Monad.Random
    ( module System.Random.MWC
    , runRandT
    , RandT
    , MonadRandom
    , getUniform
    , getUniformR
    , getUniformVector
    ) where

import System.Random.MWC
import Data.Vector.Generic (Vector)

import Control.Applicative
import Control.Monad ()
import Control.Monad.Cont
import Control.Monad.Error
import Control.Monad.Identity
import Control.Monad.Reader
import qualified Control.Monad.RWS.Lazy as RWSL
import qualified Control.Monad.RWS.Strict as RWSS
import Control.Monad.State
import qualified Control.Monad.State.Lazy as SL
import qualified Control.Monad.State.Strict as SS
import Control.Monad.Trans ()
import Control.Monad.Trans.Except
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Writer.Class
import qualified Control.Monad.Writer.Lazy as WL
import qualified Control.Monad.Writer.Strict as WS
import Control.Monad.Primitive
import Control.Monad.Catch
import Data.Monoid (Monoid)

class Monad m => MonadRandom m where
    getUniform       :: Variate a
                     => m a
    getUniformR      :: Variate a
                     => (a, a)
                     -> m a
    getUniformVector :: (Variate a, Vector v a)
                     => Int
                     -> m (v a)

newtype RandT g m a = RandT { unRandT :: ReaderT g m a }
    deriving (Functor, Applicative, Monad, MonadTrans, MonadIO, MonadFix, MonadWriter w, MonadState s, MonadThrow, MonadCatch, MonadMask)

runRandT :: (Monad m, PrimMonad m)
         => RandT (Gen (PrimState m)) m a
         -> Gen (PrimState m)
         -> m a
runRandT (RandT a) = runReaderT a

instance (Monad m, MonadIO m, GenIO ~ g) => MonadRandom (RandT g m) where
    getUniform = RandT (ReaderT (liftIO . asGenIO uniform))
    getUniformR r = RandT (ReaderT (liftIO . asGenIO (uniformR r)))
    getUniformVector n = RandT (ReaderT (liftIO . asGenIO (`uniformVector` n)))

instance (MonadReader r m) => MonadReader r (RandT g m) where
    ask = lift ask
    local r a = RandT (ReaderT (local r . runReaderT (unRandT a)))
    reader = lift . reader

instance PrimMonad m => PrimMonad (RandT g m) where
    type PrimState (RandT g m) = PrimState m
    primitive = lift . primitive

instance (MonadRandom m) => MonadRandom (ReaderT r m) where
    getUniform = lift getUniform
    getUniformR = lift . getUniformR
    getUniformVector = lift . getUniformVector

instance (MonadRandom m) => MonadRandom (IdentityT m) where
    getUniform = lift getUniform
    getUniformR = lift . getUniformR
    getUniformVector = lift . getUniformVector

instance (MonadRandom m) => MonadRandom (SL.StateT s m) where
    getUniform = lift getUniform
    getUniformR = lift . getUniformR
    getUniformVector = lift . getUniformVector

instance (MonadRandom m) => MonadRandom (SS.StateT s m) where
    getUniform = lift getUniform
    getUniformR = lift . getUniformR
    getUniformVector = lift . getUniformVector

instance (MonadRandom m, Monoid w) => MonadRandom (WL.WriterT w m) where
    getUniform = lift getUniform
    getUniformR = lift . getUniformR
    getUniformVector = lift . getUniformVector

instance (MonadRandom m, Monoid w) => MonadRandom (WS.WriterT w m) where
    getUniform = lift getUniform
    getUniformR = lift . getUniformR
    getUniformVector = lift . getUniformVector

instance (MonadRandom m, Monoid w) => MonadRandom (RWSL.RWST r w s m) where
    getUniform = lift getUniform
    getUniformR = lift . getUniformR
    getUniformVector = lift . getUniformVector

instance (MonadRandom m, Monoid w) => MonadRandom (RWSS.RWST r w s m) where
    getUniform = lift getUniform
    getUniformR = lift . getUniformR
    getUniformVector = lift . getUniformVector

instance (MonadRandom m) => MonadRandom (ExceptT e m) where
    getUniform = lift getUniform
    getUniformR = lift . getUniformR
    getUniformVector = lift . getUniformVector

instance (MonadRandom m, Error e) => MonadRandom (ErrorT e m) where
    getUniform = lift getUniform
    getUniformR = lift . getUniformR
    getUniformVector = lift . getUniformVector

instance (MonadRandom m) => MonadRandom (MaybeT m) where
    getUniform = lift getUniform
    getUniformR = lift . getUniformR
    getUniformVector = lift . getUniformVector

instance (MonadRandom m) => MonadRandom (ContT r m) where
    getUniform = lift getUniform
    getUniformR = lift . getUniformR
    getUniformVector = lift . getUniformVector

