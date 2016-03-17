
module Control.EasyFRP.WebSocket
  ( WsConn
  , wsWaitForRequest
  , wsServer
  , wsConnServe
  ) where

import Control.Concurrent.Chan
import Control.EasyFRP
import Network.WebSockets
import Control.Monad.IO.Class
import Data.Aeson

data WsConn
  = WsConn
    { pendingConnection :: PendingConnection
    , requestHead       :: RequestHead
    , connection        :: Connection
    }

wsConn :: PendingConnection -> IO WsConn
wsConn pc = WsConn pc (pendingRequest pc) <$> acceptRequest pc

newtype WsMessage = WsMessage { getMessage :: Message }
  deriving (Eq)

data Error = DataConversionError String
  deriving (Eq, Show)

wsWaitForRequest :: (MonadIO m) => Int -> FRPT m (Signal WsConn)
wsWaitForRequest port = do
    (chan, sig) <- node
    frpFork $ runServer "localhost" port $ (writeChan chan =<<) . wsConn
    return sig

wsConnServe :: (MonadIO m, ToJSON a, FromJSON a) => WsConn -> (a -> IO a) -> FRPT m (Signal a, Signal Error)
wsConnServe conn res = do
    (chanA, sigA) <- node
    (chanE, sigE) <- node
    forkLoop $ do
        mess <- receive (connection conn)
        case mess of
            ControlMessage (Close _ _) -> return ()
            ControlMessage (Pong _)    -> return ()
            ControlMessage (Ping msg)  -> send (connection conn) (ControlMessage (Pong msg))
            DataMessage (Binary msg)   -> writeChan chanE (DataConversionError $ "Binary message on\n" ++ show (requestHead conn))
            DataMessage (Text msg)     -> case eitherDecode msg of
                Left errorMsg -> writeChan chanE (DataConversionError $ errorMsg ++ " on\n" ++ show (requestHead conn))
                Right item -> do
                    writeChan chanA item
                    item' <- res item
                    send (connection conn) (DataMessage (Text (encode item')))
    return (sigA, sigE)

wsConnServeS :: (MonadIO m, ToJSON a, FromJSON a) => Signal WsConn -> (a -> IO a) -> FRPT m (Signal a, Signal Error)
wsConnServeS sigC res = do
    (chanA, sigA) <- node
    (chanE, sigE) <- node
    chanC <- subsribe sigC
    forkLoop $ do
        newConn <- readChan chanC
        error "TODO: splitting monad content"



wsServer :: (MonadIO m, ToJSON a, FromJSON a) => Int -> (a -> m a) -> FRPT m (Signal (WsConn, a), Signal (WsConn, Error), Signal WsConn)
wsServer = undefined

