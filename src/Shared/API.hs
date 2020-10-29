{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators  #-}

module Shared.API where

import           Servant
import           Shared.Types

type API = "new_game" :> ReqBody '[JSON] BoardSize :> Post '[JSON] (GameStatus NewGame)
        :<|> "make_move" :> ReqBody '[JSON] Move :> Post '[JSON] (GameStatus Board)


api :: Proxy API
api = Proxy
