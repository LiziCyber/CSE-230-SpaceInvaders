{-# LANGUAGE OverloadedStrings #-}

module Main where

import UI
import Data
import Leaderboard
import GameHandler

import Control.Monad.IO.Class
import Control.Monad (forever, void)
import Control.Concurrent (threadDelay, forkIO)
import Linear.V2 (V2(..), _x, _y)
import Control.Lens ((^.))
import Graphics.Vty as V
import Graphics.Vty.Platform.Unix (mkVty)
import Brick.BChan (newBChan, writeBChan)
import Brick
  ( App(..), BrickEvent(..), EventM
  , neverShowCursor, customMain, halt, modify, get, put
  )

-- App Initialization
app :: App AppState Tick Name
app = App
  { appDraw = drawUI
  , appChooseCursor = neverShowCursor
  , appHandleEvent = handleEvent
  , appStartEvent = pure ()
  , appAttrMap = const attributeMap
  }

-- Main entry point
main :: IO ()
main = do
  chan <- newBChan 10
  _ <- forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay 100000 -- Tick speed
  initState <- initAppState
  let builder = mkVty defaultConfig
  initialVty <- builder
  void $ customMain initialVty builder (Just chan) app initState

-- Event Handling
handleEvent :: BrickEvent Name Tick -> EventM Name AppState ()
handleEvent (AppEvent Tick) = step
handleEvent (VtyEvent (V.EvKey V.KRight [])) = continue $ moveHorizontal (+ 1)
handleEvent (VtyEvent (V.EvKey V.KLeft [])) = continue $ moveHorizontal (subtract 1)
handleEvent (VtyEvent (V.EvKey V.KUp [])) = continue $ moveVertical (+ 1)
handleEvent (VtyEvent (V.EvKey V.KDown [])) = continue $ moveVertical (subtract 1)
handleEvent (VtyEvent (V.EvKey V.KEsc [])) = quitGame
handleEvent (VtyEvent (V.EvKey (V.KChar ' ') [])) = continue shoot
handleEvent (VtyEvent (V.EvKey (V.KChar 'p') [])) = continue pause
handleEvent (VtyEvent (V.EvKey (V.KChar 'r') [])) = continue restart
handleEvent (VtyEvent (V.EvKey (V.KChar 'l') [])) = showLeaderboard
handleEvent (VtyEvent (V.EvKey V.KEnter [])) = modify startGame
handleEvent _ = pure ()

-- Game Logic Step
step :: EventM Name AppState ()
step = do
  appState <- get
  case appState of
    MenuState -> put appState
    LeaderboardState _ -> put appState
    GameState g -> if stopped g
      then put (GameState g)
      else do
        let movedShots = map (\v -> V2 (v ^._x) (v ^._y + 1)) $ shots g
        let movedAlienShots = map (\v -> V2 (v ^._x) (v ^._y - 1)) $ alienShots g
        let a = handleAliens g movedShots
        let b = handleBlocker g movedShots movedAlienShots
        let s = handleShots g movedShots
        let l = handleLives g movedAlienShots a
        let as = handleAlienShots g movedAlienShots
        let d = handleDirection g a
        let u = handleUfo g movedShots
        let sc = handleScore g movedShots
        let gUpd = g {aliens = a, ufo = u, shots = s, alienShots = as,
                      count = nextCount g, aShotCount = nextAShotCount g, ufoCount = nextUfoCount g,
                      blockers = b, alienDir = d, score = sc, lives = l}
        new_g <- liftIO $ levelUp gUpd
        put $ GameState new_g

-- Helper Functions
continue :: (Game -> Game) -> EventM Name AppState ()
continue f = do
  appState <- get
  case appState of
    MenuState -> put appState
    GameState g -> put (GameState (f g))
    LeaderboardState _ -> put appState

startGame :: AppState -> AppState
startGame MenuState = GameState initGame
startGame x = x

quitGame :: EventM Name AppState ()
quitGame = do
  appState <- get
  case appState of
    MenuState -> halt
    GameState _ -> put MenuState
    LeaderboardState _ -> put MenuState

showLeaderboard :: EventM Name AppState ()
showLeaderboard = do
  appState <- get
  case appState of
    MenuState -> do
      r <- liftIO getRecords
      put (LeaderboardState (Leaderboard {records = r, startIdx = 0}))
    GameState g -> put (GameState g)
    LeaderboardState l -> put (LeaderboardState l)
