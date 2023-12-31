{-# LANGUAGE OverloadedStrings #-}

module UI where

import Data
import Leaderboard
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Table as T
import qualified Graphics.Vty as V
import Linear.V2 (V2(..))
import Data.List (find)

import Brick
  ( Widget, AttrMap
  , vBox, hBox, withBorderStyle, str
  , attrMap, withAttr, AttrName, attrName, fg, on
  , emptyWidget, padRight, padTop, padAll, padBottom 
  , hLimit, (<+>), Padding(..), (<=>)
  )

-- | Draw the UI of the game
drawUI :: AppState -> [Widget Name]
drawUI (GameState g) =
  [ C.center $ padRight (Pad 2) (drawStats g) <+> drawGrid g ]
drawUI MenuState =
  [ C.center $
    vBox
      [ str "SPACE INVADERS" <=> str " "
      , B.borderWithLabel (str "") (str "Start Game (Press Enter)")
        <=> B.borderWithLabel (str "") (str "Leaderboard (Press L)")
        <=> B.borderWithLabel (str "") (str "Quit (Press Esc)")
      ]
  ]
drawUI (LeaderboardState l) =
  [ C.center $ T.renderTable $ T.table $ drawTable (slice low high (records l)) ranks]
  where
    low   = startIdx l
    high  = min (startIdx l + recordsPerPage) (length (records l)) 
    ranks = if low > high then [] else [low+1..high+1]

slice :: Int -> Int -> [a] -> [a]
slice start end = take (end - start) . drop start
    
drawTable :: [LeaderboardRecord] -> [Int] -> [[Widget Name]]
drawTable [] _ = [[str "Rank", str "Name", str "Score", str "Time"]]
drawTable records' ranks =
  drawTable [] [-1] ++ zipWith drawRow records' ranks
  where
    drawRow :: LeaderboardRecord -> Int -> [Widget Name]
    drawRow record rank =
      [str (show rank), str (name record), str (show (score' record)), str (show (time record))]

-- | Draw the stats- and info-box at the left
drawStats :: Game -> Widget Name
drawStats g =
  hLimit 18 $ vBox [ drawStatus $ status g
                   , drawInfoBox " Stats "
                     $ labledValue "Lives: " (show $ lives g)
                     <=> labledValue "Score: " (show $ score g)
                     <=> labledValue "Level: " (show $ lNext $ level g)
                   , drawInfoBox " Info "
                     $ str "Fire:    space"
                     <=> str "Pause:   p    "
                     <=> str "Restart: r    "
                     <=> str "Back:    esc  "
                   ]

-- | Combine a label with a score
labledValue :: String -> String -> Widget Name
labledValue l v = str l <+> str v

-- | Draw a info box with the passed contents
drawInfoBox :: String -> Widget Name -> Widget Name
drawInfoBox l w =
  withBorderStyle BS.unicodeBold
    $ B.borderWithLabel (str l)
    $ C.hCenter
    $ padAll 1
    w

-- | Draw the "GAME OVER" text
drawStatus :: Status -> Widget Name
drawStatus Won =
  withAttr gameOverAttr
    $ padTop (Pad 2) $ padBottom (Pad 2) $ C.hCenter $ str "YOU'RE A WINNER!!"
drawStatus Lost  =
  withAttr gameOverAttr
    $ padTop (Pad 2) $ padBottom (Pad 2) $ C.hCenter $ str "GAME OVER"
drawStatus _ = emptyWidget

-- | Draw the game border and each cell in it
drawGrid :: Game -> Widget Name
drawGrid g =
  withBorderStyle BS.unicodeBold
    $ B.borderWithLabel (str "Space Invaders !!!")
    $ vBox rows
  where
    rows         = [hBox $ cellsInRow r | r <- [height-1,height-2..0]]
    cellsInRow y = [drawCoord (V2 x y) | x <- [0..width-1]]
    drawCoord    = drawCell . cellAt g

-- | Attributes of game elements
canonAttr, shotAttr, emptyAttr, gameOverAttr, alienAttr, ufoAttr, blockerAttr, alienShotAttr, newAlienAttr :: AttrName
canonAttr = attrName "canonAttr"
shotAttr  = attrName "shotAttr"
alienShotAttr = attrName "alienShotAttr"
emptyAttr = attrName "emptyAttr"
alienAttr = attrName "alienAttr"
ufoAttr = attrName "ufoAttr"
blockerAttr = attrName "blockerAttr"
gameOverAttr = attrName "gameOver"
newAlienAttr = attrName "newAlienAttr"

-- | Set styling of attributes
attributeMap :: AttrMap
attributeMap = attrMap V.defAttr
  [ (canonAttr, fg V.blue `V.withStyle` V.bold)
  , (shotAttr, fg V.red `V.withStyle` V.bold)
  , (alienShotAttr, fg V.white `V.withStyle` V.bold)
  , (alienAttr, fg V.green `V.withStyle` V.bold)
  , (newAlienAttr, fg V.magenta `V.withStyle` V.bold)
  , (ufoAttr, fg V.red `V.withStyle` V.bold)
  , (gameOverAttr, fg V.red `V.withStyle` V.bold)
  , (blockerAttr, V.black `on` V.white)
  ]

alienHitPoints :: Coord -> Game -> Int
alienHitPoints c g = maybe 2 hits (find (\alien -> coord alien == c) (aliens g))
-- | Draw the different cells at the coordinate of the canvas
cellAt :: Game -> Coord -> Cell
cellAt g c
  | c `elem` newAlientLocations g && alienHitPoints c g == 1 = NewAlienHitOnceCell
  | c `elem` newAlientLocations g = NewAlienCell
  | c == canon g               = CanonCell
  | c `elem` shots g           = ShotCell
  | c `elem` alienShots g      = AlienShotCell
  | c `elem` alientLocations g = AlienCell
  | c `elem` ufoLocations g    = UfoCell
  | c `elem` blockerLocations g 3 = BlockerCell0
  | c `elem` blockerLocations g 2 = BlockerCell1
  | c `elem` blockerLocations g 1 = BlockerCell2
  | otherwise                  = EmptyCell

-- | Draw the game elemts as a 3x2 cell
drawCell :: Cell -> Widget Name
drawCell NewAlienCell =
  withAttr newAlienAttr $ str "ΩΩΩ" <=> str "πππ"
drawCell NewAlienHitOnceCell =
  withAttr newAlienAttr $ str "XXX" <=> str "πππ"
drawCell CanonCell =
  withAttr canonAttr $ str "◿⍙◺" <=> str "▞╫▚"
drawCell ShotCell =
  withAttr shotAttr $ str " ╫ " <=> str " ╫ "
drawCell AlienShotCell =
  withAttr alienShotAttr $ str " ↡ " <=> str " ◆ "
drawCell AlienCell =
  withAttr alienAttr $ str "◠◡◠" <=> str "/-\\"
drawCell UfoCell =
  withAttr ufoAttr $ str "▞█▚" <=> str "░░░"
drawCell BlockerCell0 =
  withAttr blockerAttr $ str "▓▓▓" <=> str "▓▓▓"
drawCell BlockerCell1 =
  withAttr blockerAttr $ str "▓▀▓" <=> str " ▄▓"
drawCell BlockerCell2 =
  withAttr blockerAttr $ str "▄▀▓" <=> str "▄▀▄"
drawCell EmptyCell =
  withAttr emptyAttr $ str "   " <=> str "   "
