{-# LANGUAGE OverloadedStrings #-}

module Client.UI where

import           Brick.Types
import           Brick.Widgets.Core

import           Brick.AttrMap              (attrMap)
import           Brick.Main                 (App (..), continue, halt,
                                             showFirstCursor)
import qualified Brick.Widgets.Border       as B
import           Brick.Widgets.Border.Style (BorderStyle)
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center       as C
import qualified Graphics.Vty               as V

import           Data.Text                  (Text)
import qualified Data.Text                  as T

import           Client.Requests
import           Client.Types
import           Control.Monad.IO.Class
import           Servant.Client
import           Shared.Types


-- | Draw a single Cell
drawCell :: Cell -> Bool -> Widget ()
drawCell Nothing True         = drawStyledCellWithText " " BS.unicodeBold
drawCell Nothing False        = drawStyledCellWithText " " BS.unicode
drawCell (Just PlayerX) True  = drawStyledCellWithText "X" BS.unicodeBold
drawCell (Just PlayerX) False = drawStyledCellWithText "X" BS.unicode
drawCell (Just Player0) True  = drawStyledCellWithText "0" BS.unicodeBold
drawCell (Just Player0) False = drawStyledCellWithText "0" BS.unicode


drawStyledCellWithText :: Text -> BorderStyle -> Widget ()
drawStyledCellWithText t s = withBorderStyle s
  $ B.border
  $ setAvailableSize (10, 5)
  $ C.center
  $ txt t


-- | Draw a whole board
drawListBoard :: Coords -> ListBoard -> Widget ()
drawListBoard coords (ListBoard lb) = C.center
  $ vBox
  $ foldr (\(cs, cell) acc -> drawCell cell (cs == coords) <+> acc) emptyWidget
  <$> lb


-- | Draw a title on top of the board
drawTitle :: Player -> Widget ()
drawTitle p = padTopBottom 1
  $ vLimit 10
  $ C.hCenter
  $ txt
  $ T.pack
  $ "You are " <> show p


-- | Draw result message
drawResult :: Maybe GameResult -> Widget ()
drawResult Nothing = padTopBottom 1 $ vLimit 10 $ txt ""
drawResult (Just r) = padTopBottom 1
  $ vLimit 10
  $ C.hCenter
  $ txt
  $ T.pack
  $ case r of
      PlayerWon p ->
        show p <> " won!"
      Draw ->
        "Draw."


-- | Draw exit message on the bottom of a screen
exitMessage :: Maybe GameResult -> Widget ()
exitMessage r = padTopBottom 1
  $ vLimit 10
  $ C.hCenter
  $ txt
  $ T.pack
  $ case r of
      Nothing ->
        ""
      Just _ ->
        "Press Esc key to close the game."


-- | Combine all widgets
drawUI :: GameState -> [Widget ()]
drawUI st = [ vBox [ drawTitle (userPlayer st)
                   , drawResult (gameResult st)
                   , drawListBoard (activeCell st) (listBoard st)
                   , exitMessage (gameResult st)
                   ]
            ]


-- | Handling key events
appEvent :: GameState -> BrickEvent () e -> EventM () (Next GameState)
appEvent st e =
  case (e, activeCell st) of
    (VtyEvent (V.EvKey V.KUp []), (l, c)) ->
      if l > 0 then
        continue (st { activeCell = (l - 1, c) })
      else
        continue st
    (VtyEvent (V.EvKey V.KDown []), (l, c)) ->
      if l < (bs - 1) then
        continue (st { activeCell = (l + 1, c) })
      else
        continue st
    (VtyEvent (V.EvKey V.KLeft []), (l, c)) ->
      if c > 0 then
        continue (st { activeCell = (l, c - 1) })
      else
        continue st
    (VtyEvent (V.EvKey V.KRight []), (l, c)) ->
      if c < (bs - 1) then
        continue (st { activeCell = (l, c + 1) })
      else
        continue st
    (VtyEvent (V.EvKey V.KEsc []), _) ->
      halt st
    (VtyEvent (V.EvKey V.KEnter []), _) -> do
      res <- liftIO
        $ runClientM (makeMoveClient $ Move (activeCell st) (gameToken st))
        $ mkClientEnv (globalMng st) (serverAddr st)
      case res of
        Right (GameOk b) ->
          continue (st { listBoard = boardToListBoard b })
        Right (GameFinished r b) ->
          continue (st { listBoard = boardToListBoard b, gameResult = Just r })
        _ ->
          continue st
    _ -> continue st
  where
    bs = unBoardSize $ gameBoardSize st


-- | The brick app
theApp :: App GameState e ()
theApp = App
  { appDraw = drawUI
  , appChooseCursor = showFirstCursor
  , appHandleEvent = appEvent
  , appStartEvent = return
  , appAttrMap = const $ attrMap V.defAttr []
  }
