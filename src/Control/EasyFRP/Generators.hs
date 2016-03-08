
module Control.EasyFRP.Generators where

import Control.Concurrent
import Control.Exception
import Control.Concurrent.MVar
import Control.Concurrent.Chan
import Control.Monad

import Control.Monad.State.Lazy

import Control.EasyFRP.Core
import Control.EasyFRP.Combinators


consoleIn :: (MonadIO m) => FRPT m (Signal String)
consoleIn = io getLine

stepping :: (MonadIO m) => Int -> Int -> FRPT m (Signal Int)
stepping from sec = do
    mvar <- liftIO $ newMVar from
    io (stepping' mvar sec)
  where
    stepping' :: MVar Int -> Int -> IO Int
    stepping' mvar sec = do
        i <- takeMVar mvar
        putMVar mvar (i+1)
        threadDelay (sec * 1000000)
        return i

data TimeUnit = Milisec | Sec | Minute | Hour | Day
toNum Milisec = 1000
toNum Sec = 1000000
toNum Minute = 1000000 * 60
toNum Hour = 1000000 * 60 * 60
toNum Day = 1000000 * 60 * 60 * 24

clock :: (MonadIO m) => TimeUnit -> Int -> FRPT m (Signal ())
clock unit num = io (threadDelay (toNum unit * num))

clockDo :: (MonadIO m) => TimeUnit -> Int -> IO a -> FRPT m (Signal a)
clockDo unit num m = io (threadDelay (toNum unit * num) >> m)
