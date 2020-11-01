module Server.ModelsSpec where

import Control.Monad.IO.Class
import qualified Data.Map.Strict as Map
import Data.Word (Word8)
import Server.Models
import Shared.Types
import System.Random.SplitMix
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "getGameResult Specs" $ do

    it "Diagonal 1 Win" $ do
      let diagonal = Board (BoardSize 3)
        (Map.fromList [ ((0, 0), Just PlayerX), ((0, 1), Nothing), ((0, 2), Nothing)
                      , ((1, 0), Nothing), ((1, 1), Just PlayerX), ((1, 2), Nothing)
                      , ((2, 0), Nothing), ((2, 1), Nothing), ((2, 2), Just PlayerX)
                      ])
      getGameResult diagonal `shouldBe` Just (PlayerWon PlayerX)

    it "Diagonal 2 Win" $ do
      let diagonal = Board (BoardSize 3)
        (Map.fromList [ ((0, 0), Nothing), ((0, 1), Nothing), ((0, 2), Just PlayerX)
                      , ((1, 0), Nothing), ((1, 1), Just PlayerX), ((1, 2), Nothing)
                      , ((2, 0), Just PlayerX), ((2, 1), Nothing), ((2, 2), Nothing)
                      ])
      getGameResult diagonal `shouldBe` Just (PlayerWon PlayerX)

    it "Line Win" $ do
      let line = Board (BoardSize 3)
        (Map.fromList [ ((0, 0), Nothing), ((0, 1), Nothing), ((0, 2), Nothing)
                      , ((1, 0), Just PlayerX), ((1, 1), Just PlayerX), ((1, 2), Just PlayerX)
                      , ((2, 0), Nothing), ((2, 1), Nothing), ((2, 2), Nothing)
                      ])
      getGameResult line `shouldBe` Just (PlayerWon PlayerX)


    it "Column Win" $ do
      let column = Board (BoardSize 3)
        (Map.fromList [ ((0, 0), Nothing), ((0, 1), Just PlayerX), ((0, 2), Nothing)
                      , ((1, 0), Nothing), ((1, 1), Just PlayerX), ((1, 2), Nothing)
                      , ((2, 0), Nothing), ((2, 1), Just PlayerX), ((2, 2), Nothing)
                      ])
      getGameResult column `shouldBe` Just (PlayerWon PlayerX)


  describe "makeEmptyBoard Spec" $ do
    it "Amount property" $ property $ \n ->
      Map.size (boardCells $ makeEmptyBoard (BoardSize (fromIntegral (n :: Word8)))) == fromIntegral n^2


  describe "emptyCells Spec" $ do
    it "Empty board" $ property $ \n ->
      length (emptyCells $ makeEmptyBoard (BoardSize (fromIntegral (n :: Word8)))) == fromIntegral n^2

  describe "makeRandomMove Spec" $ do
    it "make 2 random moves" $ property $ \n -> do
      let boundedN = fromIntegral ((n :: Word8) `mod` 253) + 2
      let (gen1, gen2) = (mkSMGen $ fromIntegral boundedN, mkSMGen $ fromIntegral boundedN + 1)
      let board = makeEmptyBoard $ BoardSize boundedN
      let (Right updBoard) = (makeRandomMove gen1 PlayerX board) >>= (makeRandomMove gen2 Player0)
      length (emptyCells updBoard) == boundedN^2 - 2
