
module Control.EasyFRP where

import Control.EasyFRP.Core
import Control.EasyFRP.Combinators
import Control.EasyFRP.Generators



example :: IO ()
example = runFRPT $ do
    consS <- consoleIn
    --steppingS <- stepping 0 5
    clockS <- clock Sec 5
    steppingS <- foldlF clockS 0 (const . (+1))
    concS <- mapF2 consS steppingS $ \ str i -> show i ++ ": " ++ str
    io1 concS putStrLn
    --mapFM clockS (\ () -> putStrLn "clock")
    terminateWhen steppingS (==5)

prEx :: IO ()
prEx = runFRPT $ do
    input <- rec prompt
    terminateWhen input (== "exit")
    pushF input "$"

