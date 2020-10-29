module Server.Lib where

import           Control.Monad.Reader
import           Network.Wai.Handler.Warp (run)
import           Servant
import           Server.Controllers
import           Server.Types
import           Shared.API


-- | Wai Application
serverApp :: Games -> Application
serverApp gs = serve api
  $ hoistServer api toHandler controllers
  where
    toHandler x = runReaderT (runAppM x) gs


-- | Start the server
runServer :: Int -> Games -> IO ()
runServer p gs = run p (serverApp gs)
