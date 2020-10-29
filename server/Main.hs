module Main where

import           Control.Concurrent.STM.TVar
import qualified Data.Map.Strict             as Map
import           Server.Lib
import           Server.Types
import           System.Random.SplitMix      (initSMGen)


port :: Int
port = 4000

main :: IO ()
main = do
  putStrLn $ "Starting the server at " <> show port <> " port."

  _ <- initSMGen
  gs <- newTVarIO Map.empty

  runServer port (Games gs)
