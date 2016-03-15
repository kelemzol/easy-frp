
module Control.EasyFRP.WebSocket where

import Control.EasyFRP
import Control.EasyFRP.WebSocket
import Network.WebSockets


newtype WSConn = WSConn { getPendingConnection :: PendingConnection }

newtype WsMessage = WsMessage { getMessage :: Message }


wsGetWSConn :: (MonadIO m) => Int -> FRPT m (Signal WSConn)
wsGetWSConn = undefined

wsServer :: (MonadIO m) => Int -> (WsMessage -> WsMessage) -> FRPT m (Signal (WsConn, WsMessage), Signal WSConn)
wsServer = undefined

wsConnServe :: (MonadIO m) => WSConn -> (WsMessage -> WsMessage) -> FRPT m (Signal WsMessage)
wsConnServe = undefined

