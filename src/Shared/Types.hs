{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}

module Shared.Types where

-- fromJson, toJson
import Data.Aeson
import Data.Map.Strict (Map)
-- A space efficient, packed, unboxed Unicode text type
import Data.Text
-- used for fromJSON and toJSON
import GHC.Generics


data Player
  = PlayerX | Player0
  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)


data Board = Board
  { boardSize  :: BoardSize
    -- key - Coords, value - Cell
  , boardCells :: Map Coords Cell
  } deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)


type Coords = (Int, Int)

type Cell = Maybe Player


newtype BoardSize = BoardSize { unBoardSize :: Int }
  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)


data NewGame = NewGame Board Player GameToken
  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

-- import Data.Text
newtype GameToken = GameToken Text
  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)


data GameStatus a
  = GameOk a
  | GameFinished GameResult Board
  | GameError GameError
  | GameTerminated
  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)


instance Functor GameStatus where
  fmap f (GameOk x)         = GameOk (f x)
  fmap _ (GameFinished x y) = GameFinished x y
  fmap _ (GameError e)      = GameError e


instance Applicative GameStatus where
  pure = pure
  -- (<*>) :: f (a -> b) -> f a -> f b
  (GameOk f) <*> (GameOk x) = GameOk (f x)
  (GameOk _) <*> (GameFinished x y) = GameFinished x y
  (GameOk _) <*> (GameError x) = GameError x
  (GameFinished x y) <*> _ = GameFinished x y
  (GameError x) <*> _ = GameError x


instance Monad GameStatus where
  return = GameOk
  -- (>>=) :: Monad m => m a -> (a -> m b) -> m b
  (GameOk x) >>= f = f x
  (GameFinished r b) >>= _ = GameFinished r b
  (GameError e) >>= _ = GameError e


data GameError
  = UserMoveError MoveError
  | ServerMoveError MoveError
  | GameIsNotFound
  | InvalidBoardSize
  -- import Data.Text
  | InternalError Text
  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)


data Move = Move Coords GameToken
  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)


data GameResult
  = Draw
  | PlayerWon Player
  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)


data MoveError
  = IncorrectCoords
  | CellIsAlreadyFilled
  | NoEmptyCells
  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

