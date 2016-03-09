
{-# LANGUAGE KindSignatures, GeneralizedNewtypeDeriving #-}

module Control.EasyFRP.Core where

import Control.Concurrent
import Control.Exception
import Control.Concurrent.MVar
import Control.Concurrent.Chan
import Control.Monad

import Control.Monad.State.Lazy

newtype FRPT (m :: * -> *) a = FRPT { unFRPT :: StateT FRPState m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadState FRPState)

data Signal a
  = Signal
    { subscribeChan :: (Chan (Chan a))
    , inputChan     :: (Chan a)
    }

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

subscribe :: (MonadIO m) => Signal a -> m (Chan a)
subscribe (Signal chanList _) = do
    chan <- liftIO $ newChan
    liftIO $ writeChan chanList chan
    return chan

forkLoop :: (MonadIO m) => IO a -> FRPT m ()
forkLoop m = let loop = m >> loop in frpFork (void loop)

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
    return (chan, Signal newChans chan)

pushF :: (MonadIO m) => Signal a -> a -> FRPT m ()
pushF (Signal _ chan) = liftIO . writeChan chan

(<<-) :: (MonadIO m) => Signal a -> a -> FRPT m ()
(<<-) = pushF

signalMergeF :: (MonadIO m) => Signal a -> Signal a -> FRPT m () -- Optimize issue: instead of subcribe, put dest chanel into subscribe-chanel of source signal
signalMergeF (Signal _ chan') sig = do
    chan <- subscribe sig
    forkLoop $ readChan chan >>= writeChan chan'

(<--) :: (MonadIO m) => Signal a -> Signal a -> FRPT m ()
(<--) = signalMergeF

writeMVar :: MVar a -> a -> IO ()
writeMVar mvar value = mask_ $ do
    tryTakeMVar mvar
    putMVar mvar value
