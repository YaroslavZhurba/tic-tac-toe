module Server.Lib where

import Control.Monad.Reader
import Network.Wai.Handler.Warp (run)
import Servant
import Server.Controllers
import Server.Types
import Shared.API

-- !!!
-- | Wai Application
-- Provides a common protocol for communication between web applications and web servers.
-- serve :: HasServer api '[] => Proxy api -> Server api -> Application
-- hoistServer :: HasServer api '[] => Proxy api -> (forall x. m x -> n x) -> ServerT api m -> ServerT api n
-- type Application = Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
-- type Server api = ServerT api Handler
serverApp :: Games -> Application
serverApp gs = serve api $ hoistServer api toHandler controllers
  where
    -- runAppM :: ReaderT Games Handler a
    -- newtype ReaderT r m a -- runReaderT :: r -> m a
    toHandler x = runReaderT (runAppM x) gs


-- | Start the server
-- run :: Port -> Application -> IO ()
runServer :: Int -> Games -> IO ()
runServer p gs = run p (serverApp gs)
