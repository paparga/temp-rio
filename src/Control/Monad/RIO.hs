{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Monad.RIO (RIO(..), runRIO, liftRIO) where

import Control.Monad.Reader (ReaderT(..), MonadReader, MonadIO, liftIO, ask)
import Control.Monad.Catch (MonadThrow)

newtype RIO env a = RIO { unRIO :: ReaderT env IO a }
    deriving (Functor,Applicative,Monad,MonadIO,MonadReader env,MonadThrow)
  
runRIO :: MonadIO m => env -> RIO env a -> m a
runRIO env (RIO (ReaderT f)) = liftIO (f env)
  
liftRIO :: (MonadIO m, MonadReader env m) => RIO env a -> m a
liftRIO rio = do
    env <- ask
    runRIO env rio