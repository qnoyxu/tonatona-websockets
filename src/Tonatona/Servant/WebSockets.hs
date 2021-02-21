{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeApplications #-}

module Tonatona.Servant.WebSockets
  ( run
  ) where

import RIO

import Tonatona (HasConfig(..))
import qualified Tonatona.Logger as TonaLogger
import qualified Tonatona.Servant as TonaServant
import Tonatona.Wai.WebSockets (webSocketsOr)
import qualified Tonatona.WebSockets.Unlift as TonaWebSockets

import Data.Default (def)
import Data.Kind (Type)
import Network.Wai (Middleware)
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.RequestLogger (OutputFormat(..), logStdout, logStdoutDev, mkRequestLogger, outputFormat)
import Network.Wai.Middleware.RequestLogger.JSON (formatAsJSONWithHeaders)
import Servant



-- | Run Servant server with WebSocket server.
-- This replaces @Tonatona.Servant.run@.
run ::
     forall (api :: Type) env.
     (HasServer api '[], HasConfig env TonaServant.Config, HasConfig env TonaLogger.Config)
  => (forall a. [RIO.Handler (RIO env) a])
  -> TonaWebSockets.ServerApp env
  -> ServerT api (RIO env)
  -> RIO env ()
run handlers webSocketServer servantServer = do
  env <- ask
  conf <- asks config
  loggingMiddleware <- reqLogMiddleware
  let
    app' = runServant @api env handlers servantServer
    app = webSocketsOr env TonaWebSockets.defaultConnectionOptions webSocketServer app'
  liftIO $ Warp.run (TonaServant.port conf) $ loggingMiddleware app


runServant ::
     forall (api :: Type) env. (HasServer api '[])
  => env
  -> (forall a. [RIO.Handler (RIO env) a])
  -> ServerT api (RIO env)
  -> Application
runServant env handlers servantServer =
  serve (Proxy @api) $ hoistServer (Proxy @api) (transformation . t) servantServer
  where
    t :: forall a. RIO env a -> RIO env a
    t = flip catches handlers
    transformation
      :: forall a. RIO env a -> Servant.Handler a
    transformation action = do
      let
        ioAction = Right <$> runRIO env action
#if MIN_VERSION_servant(0, 16, 0)
      eitherRes <- liftIO $ ioAction `catch` \(e :: ServerError) -> pure $ Left e
#else
      eitherRes <- liftIO $ ioAction `catch` \(e :: ServantErr) -> pure $ Left e
#endif
      case eitherRes of
        Right res -> pure res
        Left servantErr -> throwError servantErr


reqLogMiddleware :: (HasConfig env TonaLogger.Config) => RIO env Middleware
reqLogMiddleware = do
  TonaLogger.Config {mode, verbose} <- asks config
  case (mode, verbose) of
    (TonaLogger.Development, TonaLogger.Verbose True) ->
      liftIO mkLogStdoutVerbose
    (TonaLogger.Development, TonaLogger.Verbose False) ->
      pure logStdoutDev
    (_, TonaLogger.Verbose True) ->
      pure logStdoutDev
    (_, TonaLogger.Verbose False) ->
      pure logStdout


mkLogStdoutVerbose :: IO Middleware
mkLogStdoutVerbose = do
  mkRequestLogger def
    { outputFormat = CustomOutputFormatWithDetailsAndHeaders formatAsJSONWithHeaders
    }
