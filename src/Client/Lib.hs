module Client.Lib where

import qualified Brick.Main as M
import Client.Requests
import Client.Types
import Client.UI
import Control.Monad (void)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant.Client
import Shared.Types


-- | Starting the brick UI
-- defaultMain ::  Ord n
-- => App s e n	// The application.
-- -> s	// The initial application state.
-- -> IO s
runBrick :: GameState -> IO ()
runBrick gs = void $ M.defaultMain theApp gs


-- | Ask client for data for GameState
getGameState :: IO (Maybe GameState)
getGameState = do
--  putStrLn "Enter the address of server: "
--  addr <- getLine
  let addr = "127.0.0.1"

--  putStrLn "Enter the server port: "
--  sPort <- getLine
--  let port = read sPort :: Int

  let port = 4000

  -- BaseUrl :: Scheme -> String -> Int -> String -> BaseUrl
  -- Http --  http://
  -- addr
  -- port
  -- path
  let bu = BaseUrl Http addr port ""

  putStrLn "Enter the board size: "
  sBoardSize <- getLine
  let bs = read sBoardSize :: Int
  -- Keeps track of open connections for keep-alive.
  -- If possible, you should share a single Manager between multiple threads and requests.
  mng <- newManager defaultManagerSettings
 -- mkClientEnv :: Manager -> BaseUrl -> ClientEnv
 -- ClientM a -> ClientEnv -> IO (Either ClientError a)
  res <- runClientM (newGameClient $ BoardSize bs) $ mkClientEnv mng bu

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
        , errorMessage = ErrMsg ""
        }

    Right e -> do
       putStrLn $ "Server returned message: " <> show e
       pure Nothing

    Left _ -> do
      putStrLn "Unable to connect to the server."
      pure Nothing
