
module Control.EasyFRP.Combinators where

import Control.Concurrent
import Control.Exception
import Control.Concurrent.MVar
import Control.Concurrent.Chan
import Control.Monad

import Control.Monad.State.Lazy

import Control.EasyFRP.Core



rec :: (MonadIO m) => (Signal a -> FRPT m (Signal a)) -> FRPT m (Signal a)
rec gsig = do
    (chan', sig') <- node
    sig <- gsig sig'
    chan <- subscribe sig
    forkLoop $ readChan chan >>= writeChan chan'
    return sig

rec2 :: (MonadIO m) => (Signal a -> FRPT m (Signal b)) -> (Signal b -> FRPT m (Signal a)) -> FRPT m (Signal a, Signal b)
rec2 = undefined

data Alt2 a b = Alt21 a | Alt22 b
data Alt3 a b c = Alt31 a | Alt32 b | Alt33 c
data Alt4 a b c d = Alt41 a | Alt42 b | Alt43 c | Alt44 d

split2F :: (MonadIO m) => Signal a -> (a -> Alt2 b c) -> FRPT m (Signal b, Signal c)
split2F sig f = do
    chan <- subscribe sig
    (chanA, sigA) <- node
    (chanB, sigB) <- node
    forkLoop $ do
        item <- readChan chan
        case f item of
            (Alt21 i) -> writeChan chanA i
            (Alt22 i) -> writeChan chanB i
    return (sigA, sigB)

split3F :: (MonadIO m) => Signal a -> (a -> Alt3 b c d) -> FRPT m (Signal b, Signal c, Signal d)
split3F sig f = do
    chan <- subscribe sig
    (chanA, sigA) <- node
    (chanB, sigB) <- node
    (chanC, sigC) <- node
    forkLoop $ do
        item <- readChan chan
        case f item of
            (Alt31 i) -> writeChan chanA i
            (Alt32 i) -> writeChan chanB i
            (Alt33 i) -> writeChan chanC i
    return (sigA, sigB, sigC)

split4F :: (MonadIO m) => Signal a -> (a -> Alt4 b c d e) -> FRPT m (Signal b, Signal c, Signal d, Signal e)
split4F sig f = do
    chan <- subscribe sig
    (chanA, sigA) <- node
    (chanB, sigB) <- node
    (chanC, sigC) <- node
    (chanD, sigD) <- node
    forkLoop $ do
        item <- readChan chan
        case f item of
            (Alt41 i) -> writeChan chanA i
            (Alt42 i) -> writeChan chanB i
            (Alt43 i) -> writeChan chanC i
            (Alt44 i) -> writeChan chanD i
    return (sigA, sigB, sigC, sigD)

terminateWhen :: (MonadIO m) => Signal a -> (a -> Bool) -> FRPT m ()
terminateWhen sig cond = do
    chan <- subscribe sig
    st <- get
    forkLoop $ do
        item <- readChan chan
        if cond item then putMVar (terminateFlag st) () else return ()

foldrF :: (MonadIO m, Num b) => Signal a -> b -> (a -> b -> b) -> FRPT m (Signal b)
foldrF sig b f = do
    chan <- subscribe sig
    (chan', sig') <- node
    mvar <- liftIO $ newMVar b
    forkLoop $ do
        item <- readChan chan
        val <- takeMVar mvar
        let val' = f item val
        writeChan chan' val'
        putMVar mvar val'
    return sig'

foldlF :: (MonadIO m, Num b) => Signal a -> b -> (b -> a -> b) -> FRPT m (Signal b)
foldlF sig b f = foldrF sig b (flip f)


sumF :: (MonadIO m, Num a) => Signal a -> FRPT m (Signal a)
sumF sig = foldrF sig 0 (+)

mapF :: (MonadIO m) => Signal a -> (a -> b) -> FRPT m (Signal b)
mapF sig f = do
    chan <- subscribe sig
    (chan', sig') <- node
    forkLoop $ do
        item <- readChan chan
        writeChan chan' (f item)
    return sig'

mapF2 :: (MonadIO m) => Signal a  -> Signal b -> (a -> b -> c) -> FRPT m (Signal c)
mapF2 sA sB f = do
    sZ <- zip2F sA sB
    chan <- subscribe sZ
    (chan', sig') <- node
    forkLoop $ do
            (itemA, itemB) <- readChan chan
            writeChan chan' (f itemA itemB)
    return sig'

merge2F :: (MonadIO m) => Signal a -> Signal a -> FRPT m (Signal a)
merge2F sA sB = do
    chanA <- subscribe sA
    chanB <- subscribe sB
    (chan', sig) <- node
    forkLoop $ readChan chanA >>= writeChan chan'
    forkLoop $ readChan chanB >>= writeChan chan'
    return sig

zip2F :: (MonadIO m) => Signal a  -> Signal b -> FRPT m (Signal (a, b))
zip2F sA sB = do
    chanA <- subscribe sA
    chanB <- subscribe sB
    (chan', sig') <- node
    registerA <- liftIO $ newEmptyMVar
    registerB <- liftIO $ newEmptyMVar
    forkLoop $ do
            itemA <- readChan chanA
            writeMVar registerA itemA
            itemB <- readMVar registerB
            writeChan chan' (itemA, itemB)
    forkLoop $ do
            itemB <- readChan chanB
            writeMVar registerB itemB
            itemA <- readMVar registerA
            writeChan chan' (itemA, itemB)
    return sig'

filterF :: (MonadIO m) => Signal a -> (a -> Bool) -> FRPT m (Signal a)
filterF sig cond = do
    chan <- subscribe sig
    (chan', sig') <- node
    forkLoop $ do
        item <- readChan chan
        liftIO $ if cond item then writeChan chan' item else return ()
    return sig'

io :: (MonadIO m) => IO a -> FRPT m (Signal a)
io m = do
    (chan, sig) <- node
    forkLoop $ do
        item <- m
        writeChan chan item
    return sig

io1 :: (MonadIO m) => Signal a -> (a -> IO b) -> FRPT m (Signal b)
io1 sig m = do
    chan <- subscribe sig
    (chan', sig') <- node
    forkLoop $ do
        item <- readChan chan
        item' <- m item
        writeChan chan' item'
    return sig'

