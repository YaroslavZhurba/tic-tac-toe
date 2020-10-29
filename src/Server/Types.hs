{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Server.Types where

import           Control.Concurrent.STM.TVar
import           Control.Monad.Reader
import           Data.Map.Strict             (Map)
import           Servant
import           Shared.Types


data GameState = GameState
  { userPlayer   :: Player
  , serverPlayer :: Player
  , gameBoard    :: Board
  } deriving (Eq)


newtype Games = Games { gamesMap :: TVar (Map GameToken GameState) }


newtype AppM a = AppM { runAppM :: ReaderT Games Handler a }
  deriving (Functor)
  deriving newtype (
             Applicative
           , Monad
           , MonadReader Games
           , MonadIO )


data Step
  = MaxStep
  | MinStep
  deriving (Eq)


data MoveOwner
  = User
  | Server
  deriving (Eq)
