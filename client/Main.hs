module Main where

import Client.Lib

main :: IO ()
main = do
  mbGS <- getGameState

  case mbGS of
    Just gs -> runBrick gs
    Nothing -> pure ()
