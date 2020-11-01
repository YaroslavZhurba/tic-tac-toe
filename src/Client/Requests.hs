module Client.Requests where

import Servant.API
import Servant.Client
import Shared.API
import Shared.Types

-- newtype ClientM a -- ClientM is the monad in which client functions run. Contains the Manager and BaseUrl used for requests in the reader environment.
newGameClient :: BoardSize -> ClientM (GameStatus NewGame)
makeMoveClient :: Move -> ClientM (GameStatus Board)
deleteGameClient :: GameToken -> ClientM (GameStatus ())

-- | Servant client functions
-- client :: HasClient ClientM api => Proxy api -> Client ClientM api -- Generates a set of client functions for an API.
newGameClient :<|> makeMoveClient :<|> deleteGameClient = client api
