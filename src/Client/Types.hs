module Client.Types where

import           Data.List           (groupBy)
import qualified Data.Map.Strict     as Map
import           Network.HTTP.Client (Manager)
import           Servant.Client
import           Shared.Types


-- | List board is more efficient data structure for rendering
newtype ListBoard = ListBoard [[(Coords, Cell)]]


-- | Convert Board to ListBoard
boardToListBoard :: Board -> ListBoard
boardToListBoard (Board _ cellsMap) = ListBoard
  $ groupBy (\((l, _),_) ((l', _), _) -> l == l')
  $ Map.toList cellsMap


-- | Client state of the game
data GameState = GameState
  { activeCell    :: Coords
  , gameBoardSize :: BoardSize
  , userPlayer    :: Player
  , gameResult    :: Maybe GameResult
  , gameToken     :: GameToken
  , listBoard     :: ListBoard
  , globalMng     :: Manager
  , serverAddr    :: BaseUrl
  }
