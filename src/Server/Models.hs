module Server.Models where

import           Crypto.Hash
import           Data.Bifunctor
import           Data.ByteString         (ByteString)
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Lazy    as BS (toStrict)
import           Data.Either             (isRight)
import           Data.List               (groupBy, maximumBy, minimumBy, sortBy)
import qualified Data.Map.Strict         as Map
import           Data.Maybe              (isJust, isNothing)
import qualified Data.Text               as T
import           Server.Types
import           Shared.Types
import           System.Random.SplitMix


-- | Determine game result. Returns Nothing if game still in progress
getGameResult :: Board -> Maybe GameResult
getGameResult board
  | isPlayerXWon = Just $ PlayerWon PlayerX
  | isPlayer0Won = Just $ PlayerWon Player0
  | isDraw = Just Draw
  | otherwise = Nothing
  where
    lines' = map snd
      <$> groupBy (\((l, _), _) ((l', _), _) -> l == l')
      (Map.toList $ boardCells board)
    columns = map snd
      <$> groupBy (\((_, c), _) ((_, c'), _) -> c == c')
      (sortBy (\((_, c), _) ((_, c'), _) -> compare c c')
       $ Map.toList $ boardCells board)
    diagonals = [ map snd
                        $ filter (\((l, c), _) -> l == c)
                        $ Map.toList $ boardCells board
                , map snd
                        $ filter (\((l, c), _) -> l == bs - c - 1)
                        $ Map.toList $ boardCells board
                ]
    (BoardSize bs) = boardSize board
    isPlayerXWon = or
      $ all (== Just PlayerX) <$> (lines' <> columns <> diagonals)
    isPlayer0Won = or
      $ all (== Just Player0) <$> (lines' <> columns <> diagonals)
    isDraw = all (isJust . snd)
      $ Map.toList $ boardCells board


-- | Find coordinates of empty cells
emptyCells :: Board -> [Coords]
emptyCells board = map fst
  $ filter (isNothing . snd)
  $ Map.toList
  $ boardCells board


-- | Make move into the random free cell
makeRandomMove :: SMGen -> Player -> Board -> Either MoveError Board
makeRandomMove gen player board = makeMove player randomCell board
  where
    randomCell = emptyCells board !! fromIntegral
                                       ( fst
                                       $ nextInteger 0 (fromIntegral
                                                        ( length
                                                        $ emptyCells board) - 1) gen)


-- | Make Specified Move
makeMove :: Player -> Coords -> Board -> Either MoveError Board
makeMove p coords@(l, c) board@(Board (BoardSize bs) bc) =
  case (Map.lookup coords bc, isCorrectCoords) of
    (Just Nothing, True) -> Right
      $ board { boardCells = Map.adjust (const $ Just p) coords bc }
    (_, False) -> Left IncorrectCoords
    (Nothing, _) -> Left IncorrectCoords
    (Just (Just _), _) -> Left CellIsAlreadyFilled
  where
    isCorrectCoords = l >= 0 && c >= 0 && l < bs && c < bs


makeEmptyBoard :: BoardSize -> Board
makeEmptyBoard bs'@(BoardSize bs) = Board bs'
  $ Map.fromList $ do
        l <- [0..(bs - 1)]
        c <- [0..(bs - 1)]
        pure ((l, c), Nothing)


genPlayersPair :: SMGen -> ((Player, Player), SMGen)
genPlayersPair =
  first (\x -> if even x then (Player0, PlayerX) else (PlayerX, Player0))
  . nextInt


genGameToken :: SMGen -> (GameToken, SMGen)
genGameToken = first ( GameToken
                     . T.pack
                     . show
                     . (hash :: ByteString -> Digest SHA256)
                     . BS.toStrict
                     . BS.toLazyByteString
                     . BS.word64BE ) . nextWord64
