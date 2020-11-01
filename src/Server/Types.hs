{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Server.Types where

import Control.Concurrent.STM.TVar
import Control.Monad.Reader
import Data.Map.Strict (Map)
import Servant
import Shared.Types


data GameState = GameState
  { userPlayer   :: Player
  , serverPlayer :: Player
  , gameBoard    :: Board
  } deriving (Eq)

-- Shared memory locations that support atomic memory transactions.
newtype Games = Games { gamesMap :: TVar (Map GameToken GameState) }

-- runHandler' :: ExceptT ServerError IO a
-- newtype ReaderT r m a -- runReaderT :: r -> m a
-- newtype Handler a
-- runHandler :: Handler a -> IO (Either ServerError a)
newtype AppM a = AppM { runAppM :: ReaderT Games Handler a }
  deriving (Functor)
  deriving newtype ( Applicative, Monad, MonadReader Games, MonadIO )


data MoveOwner = User | Server deriving (Eq)
