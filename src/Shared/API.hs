{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators  #-}

module Shared.API where

import Servant
import Shared.Types
-- :<|> is an Union of two APIs, first takes precedence in case of overlap.
type API = "new_game" :> ReqBody '[JSON] BoardSize :> Post '[JSON] (GameStatus NewGame)
        :<|> "make_move" :> ReqBody '[JSON] Move :> Post '[JSON] (GameStatus Board)
        :<|> "delete_game" :> ReqBody '[JSON] GameToken :> Post '[JSON] (GameStatus ())


api :: Proxy API
api = Proxy
