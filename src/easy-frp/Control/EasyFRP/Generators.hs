
{-# LANGUAGE FlexibleInstances #-}

module Control.EasyFRP.Generators where

import Control.Concurrent
import Control.Exception
import Control.Concurrent.MVar
import Control.Concurrent.Chan
import Control.Monad

import Control.Monad.State.Lazy

import Control.EasyFRP.Core
import Control.EasyFRP.Combinators


data TimeUnit = Milisec | Sec | Minute | Hour | Day deriving (Show)
data Time = Time Integer TimeUnit deriving (Show)
toNum :: (Num a) => TimeUnit -> a
toNum Milisec = 1000
toNum Sec = 1000000
toNum Minute = 1000000 * 60
toNum Hour = 1000000 * 60 * 60
toNum Day = 1000000 * 60 * 60 * 24
time :: (Num a) => Time -> a
time (Time num unit) = fromInteger (toNum unit * num)

instance Num (TimeUnit -> Time) where
    fromInteger = Time
    (+) = undefined
    (-) = undefined
    (*) = undefined
    signum = undefined
    abs = undefined

stdInNewLine :: (MonadIO m) => FRPT m (Signal String)
stdInNewLine = io getLine

stdInNewChar :: (MonadIO m) => FRPT m (Signal Char)
stdInNewChar = do
    (chan, sig) <- node
    forkLoop $ do
        l <- getLine
        forM_ l (writeChan chan)
    return sig

clockedForward :: (MonadIO m) => Signal clock -> Signal a -> FRPT m (Signal a)
clockedForward sigA sigB = do
    chanA <- subscribe sigA
    chanB <- subscribe sigB
    (chan, sig) <- node
    forkLoop $ do
        val <- readChan chanB
        _ <- readChan chanA
        writeChan chan val
    return sig

stepping :: (MonadIO m, Num a, Integral a) => a -> a -> TimeUnit -> a -> FRPT m (Signal a)
stepping from much unit num = do
    mvar <- liftIO $ newMVar from
    io (stepping' mvar much (toNum unit * num))
  where
    stepping' :: (Num a, Integral a) => MVar a -> a -> a -> IO a
    stepping' mvar much t = do
        i <- takeMVar mvar
        putMVar mvar (i+much)
        threadDelay . fromInteger . fromIntegral $ t
        return i


clock :: (MonadIO m) => TimeUnit -> Int -> FRPT m (Signal ())
clock unit num = io (threadDelay (toNum unit * num))

clockDo :: (MonadIO m) => TimeUnit -> Int -> IO a -> FRPT m (Signal a)
clockDo unit num m = io (threadDelay (toNum unit * num) >> m)

prompt :: (MonadIO m) => Signal String -> FRPT m (Signal String)
prompt sig = do
    chan <- subscribe sig
    (chan', sig') <- node
    forkLoop $ do
        pr <- readChan chan
        putStr (pr ++ " ")
        input <- getLine
        writeChan chan' input
    return sig'

