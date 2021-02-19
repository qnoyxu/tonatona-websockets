module TonaApp.Main where

import Tonalude
import TonaParser ((.||), argLong, envVar, requiredVal)
import Tonatona (HasConfig(..), HasParser(..))
import qualified Tonatona.Logger as TonaLogger
import qualified Tonatona.WebSockets as TonaWebSockets

import UnliftIO.MVar (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)
import Network.HTTP.Types (status200)
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp



-- App


app :: RIO Config ()
app = do
  port <- fromEnum <$> asks port
  TonaLogger.logDebug $
    ("About to run web server on port " <> display port <> " ...")
  newState <- liftIO $ newMVar newServerState
  env <- ask
  liftIO $ Warp.run port $ applicationWith env newState



-- Web server


application' :: Wai.Application
application' _ respond =
  respond $ Wai.responseLBS
    status200
    [("Content-Type", "text/plain")]
    "hello, HTTP server is running"


applicationWith :: Config -> MVar ServerState -> Wai.Application
applicationWith conf state =
  TonaWebSockets.webSocketsOr
    conf
    TonaWebSockets.defaultConnectionOptions
    (serverApp state)
    application'



-- WebSocket


type ClientId = Int


type Client = (ClientId, TonaWebSockets.Connection)


type ServerState = [Client]


newServerState :: ServerState
newServerState = []


numClients :: ServerState -> ClientId
numClients = length


clientExists :: Client -> ServerState -> Bool
clientExists client = any ((== fst client) . fst)


addClient :: Client -> ServerState -> ServerState
addClient client clients = client : clients


removeClient :: Client -> ServerState -> ServerState
removeClient client = filter ((/= fst client) . fst)


broadcast :: Text -> ServerState -> RIO Config ()
broadcast message clients = do
  forM_ clients $ \(_, conn) -> TonaWebSockets.sendTextData conn message


serverApp :: MVar ServerState -> TonaWebSockets.ServerApp Config
serverApp state pending = do
  conn <- TonaWebSockets.acceptRequest pending
  clients <- readMVar state
  let
    client     = (numClients clients, conn)
    disconnect = do
     -- Remove client and return new state
      s <- modifyMVar state $ \s ->
        let s' = removeClient client s in return (s', s')
      broadcast (tshow (fst client) <> " disconnected") s
  flip finally disconnect $ do
      modifyMVar_ state $ \s -> do
        let s' = addClient client s
        broadcast ("No." <> tshow (fst client) <> " joined") s'
        return s'
      TonaWebSockets.withPingThread conn 30 (return ()) $ do
        talk client state


talk :: Client -> MVar ServerState -> RIO Config ()
talk (_, conn) state = forever $ do
  msg <- TonaWebSockets.receiveData conn
  TonaLogger.logDebug $ display
    ("Received message is \"" <> msg <> "\".")
  readMVar state >>= broadcast msg



-- Config


data Config = Config
  { tonaLogger :: TonaLogger.Config
  , port :: Port
  }


newtype Port = Port Int
  deriving (Show, Enum)

instance HasParser Port where
  parser = Port <$>
    requiredVal
      "Port to run server"
      (argLong "port" .|| envVar "PORT")


instance HasConfig Config TonaLogger.Config where
  config = tonaLogger


instance HasParser Config where
  parser = Config
      <$> parser
      <*> parser
