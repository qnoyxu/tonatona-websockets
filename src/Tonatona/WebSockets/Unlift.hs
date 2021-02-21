-- | Lifted version of "Network.WebSockets"
module Tonatona.WebSockets.Unlift
  ( ServerApp
  , runServer
  , runServerWithOptions
  , acceptRequest
  , acceptRequestWith
  , rejectRequest
  , rejectRequestWith
  , receiveData
  , sendTextData
  , withPingThread
  -- Reexports
  , WS.PendingConnection
  , WS.pendingRequest
  , WS.AcceptRequest(..)
  , WS.defaultAcceptRequest
  , WS.RejectRequest(..)
  , WS.defaultRejectRequest
  , WS.Connection
  , WS.ConnectionOptions (..)
  , WS.defaultConnectionOptions
  , WS.Headers
  , WS.Request (..)
  , WS.RequestHead (..)
  , WS.getRequestSubprotocols
  , WS.Response (..)
  , WS.ResponseHead (..)
  , WS.WebSocketsData(..)
  ) where

import RIO

import qualified Network.WebSockets as WS



type ServerApp env = WS.PendingConnection -> RIO env ()


runServer :: String -> Int -> ServerApp env -> RIO env ()
runServer host port =
  runServerWithOptions
    WS.defaultServerOptions
      { WS.serverHost = host
      , WS.serverPort = port
      }


runServerWithOptions :: WS.ServerOptions -> ServerApp env -> RIO env ()
runServerWithOptions opts server =
  withRunInIO $ \run -> WS.runServerWithOptions opts (run . server)


acceptRequest :: WS.PendingConnection -> RIO env WS.Connection
acceptRequest = liftIO . WS.acceptRequest


acceptRequestWith :: WS.PendingConnection -> WS.AcceptRequest -> RIO env WS.Connection
acceptRequestWith conn = liftIO . WS.acceptRequestWith conn


rejectRequest :: WS.PendingConnection -> ByteString -> RIO env ()
rejectRequest conn = liftIO . WS.rejectRequest conn


rejectRequestWith :: WS.PendingConnection -> WS.RejectRequest -> RIO env ()
rejectRequestWith conn = liftIO . WS.rejectRequestWith conn


receiveData :: ( WS.WebSocketsData a) => WS.Connection -> RIO env a
receiveData = liftIO . WS.receiveData


sendTextData :: (WS.WebSocketsData a) => WS.Connection -> a -> RIO env ()
sendTextData conn = liftIO . WS.sendTextData conn


withPingThread :: WS.Connection -> Int -> RIO env () -> RIO env a -> RIO env a
withPingThread conn n action app =
  withRunInIO $ \run -> WS.withPingThread conn n (run action) (run app)
