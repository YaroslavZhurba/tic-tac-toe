module Client.Lib where

import qualified Brick.Main          as M
import           Client.Requests
import           Client.Types
import           Client.UI
import           Control.Monad       (void)
import           Network.HTTP.Client (defaultManagerSettings, newManager)
import           Servant.Client
import           Shared.Types


-- | Starting the brick UI
runBrick :: GameState -> IO ()
runBrick gs = void
  $ M.defaultMain theApp gs


-- | Ask client for data for GameState
getGameState :: IO (Maybe GameState)
getGameState = do
  putStrLn "Enter the address of server: "
  addr <- getLine
  putStrLn "Enter the server port: "
  sPort <- getLine
  let port = read sPort :: Int
  let bu = BaseUrl Http addr port ""
  putStrLn "Enter the board size: "
  sBoardSize <- getLine
  let bs = read sBoardSize :: Int
  mng <- newManager defaultManagerSettings
  res <- runClientM (newGameClient $ BoardSize bs)
    $ mkClientEnv mng bu
  case res of
    Right (GameOk (NewGame board player token)) -> do
      putStrLn "Successfully connected to the server."
      pure $ Just $ GameState
        { activeCell = (0, 0)
        , gameBoardSize = BoardSize bs
        , userPlayer = player
        , gameResult = Nothing
        , gameToken = token
        , listBoard = boardToListBoard board
        , globalMng = mng
        , serverAddr = bu
        }
    Right e -> do
       putStrLn $ "Server returned message: " <> show e
       pure Nothing
    Left _ -> do
      putStrLn "Unable to connect to the server."
      pure Nothing
