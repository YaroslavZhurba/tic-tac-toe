module Client.Requests where

import           Servant.API
import           Servant.Client
import           Shared.API
import           Shared.Types

newGameClient :: BoardSize -> ClientM (GameStatus NewGame)
makeMoveClient :: Move -> ClientM (GameStatus Board)

-- | Servant client functions
newGameClient :<|> makeMoveClient = client api
