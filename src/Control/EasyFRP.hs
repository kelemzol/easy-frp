
module Control.EasyFRP where

import Control.Concurrent
import Control.Exception
import Control.Concurrent.MVar
import Control.Concurrent.Chan
import Control.Monad.IO.Class
import Control.Monad

import Control.Monad.State.Lazy

import Control.EasyFRP.Core
import Control.EasyFRP.Combinators
import Control.EasyFRP.Generators



example :: IO ()
example = runFRPT $ do
    consS <- consoleIn
    --steppingS <- stepping 0 5
    clockS <- clock Sec 5
    steppingS <- foldF clockS 0 (\ () i -> i + 1)
    concS <- mapF2 consS steppingS $ \ str i -> show i ++ ": " ++ str
    mapFM concS putStrLn
    --mapFM clockS (\ () -> putStrLn "clock")
    terminateWhen steppingS (==5)



