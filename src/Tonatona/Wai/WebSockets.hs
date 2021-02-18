module Tonatona.Wai.WebSockets
  ( webSocketsOr
  ) where

import RIO

import qualified Tonatona.WebSockets as TonaWebSockets

import Network.Wai
import qualified Network.Wai.Handler.WebSockets as WWS



webSocketsOr :: env -> TonaWebSockets.ConnectionOptions -> TonaWebSockets.ServerApp env -> Application -> Application
webSocketsOr env connectionOptions server application =
  WWS.websocketsOr connectionOptions (runRIO env . server) application
