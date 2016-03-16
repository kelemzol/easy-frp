
module Control.EasyFRP.WebSocket
  ( WsConn
  , wsGetWSConn
  , wsServer
  , wsConnServe
  ) where

import Control.Concurrent.Chan
import Control.EasyFRP
import Network.WebSockets
import Control.Monad.IO.Class
import Data.Aeson

newtype WsConn = WsConn { getPendingConnection :: PendingConnection }

newtype WsMessage = WsMessage { getMessage :: Message }
  deriving (Eq)

data Error = DataConversionError String
  deriving (Eq, Show)

wsGetWSConn :: (MonadIO m) => Int -> FRPT m (Signal WsConn)
wsGetWSConn port = do
    (chan, sig) <- node
    frpFork $ runServer "localhost" port $ writeChan chan . WsConn
    return sig

wsMessages :: (MonadIO m, ToJSON a, FromJSON a) => WsConn -> FRPT m (Signal a)
wsMessages = undefined

wsConnServeInternal :: (MonadIO m) => WsConn -> (WsMessage -> m WsMessage) -> FRPT m (Signal WsMessage)
wsConnServeInternal = undefined

wsServerInternal :: (MonadIO m) => Int -> (WsMessage -> m WsMessage) -> FRPT m (Signal (WsConn, WsMessage), Signal WsConn)
wsServerInternal = undefined

wsConnServe :: (MonadIO m, ToJSON a, FromJSON a) => WsConn -> (a -> m a) -> FRPT m (Signal a, Signal Error)
wsConnServe = undefined

wsServer :: (MonadIO m, ToJSON a, FromJSON a) => Int -> (a -> m a) -> FRPT m (Signal (WsConn, a), Signal (WsConn, Error), Signal WsConn)
wsServer = undefined

