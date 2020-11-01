module Server.Controllers where

import Control.Concurrent.STM.TVar
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.STM (atomically)
import Data.Bifunctor
import qualified Data.Map.Strict as Map
import Servant
import Server.Models
import Server.Types
import Shared.API
import Shared.Types
import System.Random.SplitMix

-- | Servant controllers a.k.a. Handlers
-- type ServerT api (m :: * -> *) :: * ----- type family
-- !!!
controllers :: ServerT API AppM
controllers = newGameController :<|> makeMoveController :<|> deleteGameController


-- | Controller for a new game
newGameController :: BoardSize -> AppM (GameStatus NewGame)
newGameController bs@(BoardSize bs') = do
  -- Derive a new generator instance from the global SMGen using splitSMGen.
  -- IO SMGen
  -- liftIO :: IO a -> m a
  gen <- liftIO newSMGen

  let ((userP, serverP), gen') = genPlayersPair gen
  let (token, gen'') = genGameToken gen'
  let emptyBoard = makeEmptyBoard bs

  -- newtype Games = Games { gamesMap :: TVar (Map GameToken GameState) }
  -- asks :: Monad m => (r -> a) -> ReaderT r m a
  tGames <- asks gamesMap

  case (bs' > 1, serverP, bs') of
    (False, _, _) -> pure $ GameError InvalidBoardSize

    (True, Player0, _) -> do
      let gs = GameState userP serverP emptyBoard
      -- liftIO :: IO a -> m a -- because of monad AppM
      -- atomically :: STM a -> IO a -- (make a transaction) Perform a series of STM actions atomically.
      -- modifyTVar :: TVar a -> (a -> a) -> STM ()
      liftIO $ atomically $ modifyTVar tGames (Map.insert token gs)

      pure $ GameOk $ NewGame emptyBoard userP token

    (True, PlayerX, 1) -> pure $ GameFinished (PlayerWon PlayerX) emptyBoard

    (True, PlayerX, _) ->
      case makeRandomMove gen'' serverP emptyBoard of
        (Right b) -> do
          let gs = GameState userP serverP b
          -- liftIO :: IO a -> m a -- because of monad AppM
          liftIO $ atomically $ modifyTVar tGames (Map.insert token gs)
          pure $ GameOk $ NewGame b userP token
        (Left e) ->
          pure $ GameError $ ServerMoveError e


-- | Controller for a move
makeMoveController :: Move -> AppM (GameStatus Board)
makeMoveController (Move coords token) = do
  -- MonadReader Games,
  -- asks :: Monad m => (r -> a) -> ReaderT r m a
  tGames <- asks gamesMap
  -- deriving
  --  readTVarIO = atomically . readTVar
  games <- liftIO $ readTVarIO tGames
  -- newSMGen -- Derive a new generator instance from the global SMGen using splitSMGen.
  -- using for random move
  gen <- liftIO newSMGen

  case Map.lookup token games of
    Nothing -> pure $ GameError GameIsNotFound

    Just game -> do
      let player = userPlayer game
      let board = gameBoard game

      let userMove = updateBoardHelper User board (makeMove player coords)
      let serverMove = userMove >>= (\b -> updateBoardHelper Server b $ makeRandomMove gen (serverPlayer game))

      case serverMove of
        GameOk b -> do
          liftIO $ atomically $ modifyTVar tGames $ Map.adjust (const $ game { gameBoard = b }) token
          pure $ GameOk b

        GameFinished r b -> do
          liftIO $ atomically $ modifyTVar tGames $ Map.delete token
          pure $ GameFinished r b

        GameError e ->
          pure $ GameError e


updateBoardHelper :: MoveOwner -> Board -> (Board -> Either MoveError Board) -> GameStatus Board
updateBoardHelper own board fUpd = do
  let res = fUpd board

  case res of
    Left e -> GameError $ err e

    Right b ->
      case getGameResult b of
        Nothing -> GameOk b
        Just r -> GameFinished r b

  where
    err = if own == User then UserMoveError else ServerMoveError

-- | Controller for deleting game after esc
deleteGameController :: GameToken -> AppM (GameStatus ())
deleteGameController token = do
  -- MonadReader Games,
  -- asks :: Monad m => (r -> a) -> ReaderT r m a
  tGames <- asks gamesMap
  -- deriving
  --  readTVarIO = atomically . readTVar
  games <- liftIO $ readTVarIO tGames

  case Map.lookup token games of
    Nothing -> pure $ GameError GameIsNotFound

    Just game -> do
      liftIO $ atomically $ modifyTVar tGames $ Map.delete token
      liftIO $ putStrLn "Game deleted"
      pure $ GameTerminated
