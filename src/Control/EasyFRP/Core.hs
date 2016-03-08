
{-# LANGUAGE KindSignatures, GeneralizedNewtypeDeriving #-}

module Control.EasyFRP.Core where

import Control.Concurrent
import Control.Exception
import Control.Concurrent.MVar
import Control.Concurrent.Chan
import Control.Monad.IO.Class
import Control.Monad

import Control.Monad.State.Lazy

newtype FRPT (m :: * -> *) a = FRPT { unFRPT :: StateT FRPState m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadState FRPState)

--data Signal a = Signal (MVar [Chan a])
data Signal a = Signal (Chan (Chan a))

data FRPState
  = FRPState
    { threads        :: MVar [ThreadId]
    , terminateFlag  :: MVar ()
    }

emptyFRPState :: (MonadIO m) => m FRPState
emptyFRPState = FRPState <$> liftIO (newMVar []) <*> liftIO newEmptyMVar

frpFork :: (MonadIO m) => IO () -> FRPT m ()
frpFork m = do
    st <- get
    thId <- liftIO $ forkIO m
    liftIO $ modifyMVar_ (threads st) $ \ ths -> return (thId:ths)


naiveRunFRPT :: (MonadIO m) => FRPT m a -> m (a, FRPState)
naiveRunFRPT frpt = join $ runStateT (unFRPT frpt) <$> emptyFRPState

runFRPT :: (MonadIO m) => FRPT m a -> m a
runFRPT frpt = do
    (res, env) <- naiveRunFRPT frpt
    waitForTerminate (terminateFlag env)
    killThreads (threads env)
    return res
  where
    waitForTerminate :: (MonadIO m) => MVar () -> m ()
    waitForTerminate = void . liftIO . takeMVar
    killThreads :: (MonadIO m) => MVar [ThreadId] -> m ()
    killThreads thrs = liftIO $ mapM_ (forkIO . killThread) =<< takeMVar thrs

subscibe :: (MonadIO m) => Signal a -> m (Chan a)
subscibe (Signal chanList) = do
    chan <- liftIO $ newChan
    liftIO $ writeChan chanList chan
    return chan

forkLoop :: (MonadIO m) => IO a -> FRPT m ()
forkLoop m = let loop = m >> loop in frpFork (void loop)

mapFM :: (MonadIO m) => Signal a -> (a -> IO b) -> FRPT m (Signal b)
mapFM sig m = do
    chan <- subscibe sig
    (chan', sig') <- node
    forkLoop $ do
        item <- readChan chan
        item' <- m item
        writeChan chan' item'
    return sig'

node :: (MonadIO m) => FRPT m (Chan a, Signal a)
node = do
    chan <- liftIO $ newChan
    chanList <- liftIO $ newMVar []
    newChans <- liftIO $ newChan
    value <- liftIO $ newEmptyMVar
    forkLoop $ do
        lastValue <- readMVar value
        newChan <- readChan newChans
        writeChan newChan lastValue
        list <- takeMVar chanList
        putMVar chanList (newChan:list)
    forkLoop $ do
        item <- readChan chan
        list <- readMVar chanList
        writeMVar value item
        forM_ list $ \ chan -> do
            writeChan chan item
    return (chan, Signal newChans)

writeMVar :: MVar a -> a -> IO ()
writeMVar mvar value = mask_ $ do
    tryTakeMVar mvar
    putMVar mvar value
