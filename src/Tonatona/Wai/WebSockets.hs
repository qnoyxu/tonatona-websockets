module Tonatona.Wai.WebSockets
  ( webSocketsOr
  ) where

import RIO

import qualified Tonatona.WebSockets.Unlift as TonaWebSockets

import Network.Wai
import qualified Network.Wai.Handler.WebSockets as WWS



-- This replaces @Network.Wai.Handler.WebSockets.websocketsOr@.
webSocketsOr :: env -> TonaWebSockets.ConnectionOptions -> TonaWebSockets.ServerApp env -> Application -> Application
webSocketsOr env connectionOptions server application =
  WWS.websocketsOr connectionOptions (runRIO env . server) application
