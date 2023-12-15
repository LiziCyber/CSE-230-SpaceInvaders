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

-- | Initialize the app
app :: App AppState Tick Name
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = pure ()
          , appAttrMap = const attributeMap
          }

-- | Main entry point, set tickspeed and start the game
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

-- | Handle gameticks and keyboard inputs
handleEvent :: BrickEvent Name Tick -> EventM Name AppState ()
handleEvent (AppEvent Tick)                       = step  
handleEvent (VtyEvent (V.EvKey V.KRight []))      = do
                                                      appState <- get
                                                      case appState of 
                                                        MenuState -> put appState 
                                                        -- DialogState d -> put appState 
                                                        GameState _ -> continue $ moveHorizontal (+ 1)
                                                        LeaderboardState l -> put $ LeaderboardState (l {startIdx = let idx = startIdx l in if idx+recordsPerPage >= length (records l) then idx else idx+recordsPerPage})
handleEvent (VtyEvent (V.EvKey V.KLeft []))       = do
                                                      appState <- get
                                                      case appState of 
                                                        MenuState -> put appState 
                                                        -- DialogState d -> put appState 
                                                        GameState _ -> continue $ moveHorizontal (subtract 1)
                                                        LeaderboardState l -> put $ LeaderboardState (l {startIdx = max 0 (startIdx l - recordsPerPage)})
handleEvent (VtyEvent (V.EvKey V.KUp []))         = continue $ moveVertical (+ 1)
handleEvent (VtyEvent (V.EvKey V.KDown []))       = continue $ moveVertical (subtract 1)
handleEvent (VtyEvent (V.EvKey V.KEsc []))        = quitGame
handleEvent (VtyEvent (V.EvKey (V.KChar ' ') [])) = continue shoot
handleEvent (VtyEvent (V.EvKey (V.KChar 'p') [])) = continue pause
handleEvent (VtyEvent (V.EvKey (V.KChar 'r') [])) = continue restart
handleEvent (VtyEvent (V.EvKey (V.KChar 'l') [])) = showLeaderboard
handleEvent (VtyEvent (V.EvKey V.KEnter []))      = modify startGame
handleEvent _                                     = pure ()

step :: EventM Name AppState ()
step = do
        appState <- get
        case appState of
          MenuState -> put appState
          LeaderboardState _ -> put appState 
          -- DialogState d -> put appState 
          GameState g ->  if stopped g then put (GameState g)
                          else do
                          let movedShots = map (\v -> V2 (v ^._x) (v ^._y + 1)) $ shots g -- move shots
                          let movedAlienShots = map (\v -> V2 (v ^._x) (v ^._y - 1)) $ alienShots g -- move alien shots
                          let a = handleAliens g movedShots -- update aliens
                          let b = handleBlocker g movedShots movedAlienShots -- update blockers 
                          let s = handleShots g movedShots -- update shots
                          let l = handleLives g movedAlienShots a -- update lives
                          let as = handleAlienShots g movedAlienShots -- update alien shots
                          let d = handleDirection g a -- update alien direction
                          let u = handleUfo g movedShots -- update ufo
                          let sc = handleScore g movedShots -- update score
                          let gUpd = g {aliens = a, ufo = u, shots = s, alienShots = as,
                                count = nextCount g, aShotCount = nextAShotCount g, ufoCount = nextUfoCount g,
                                blockers = b, alienDir = d, score = sc ,lives = l}
                          new_g <- liftIO $ levelUp gUpd -- update level
                          put $ GameState new_g

continue :: (Game -> Game) -> EventM Name AppState ()
continue f = do
              appState <- get
              case appState of
                MenuState -> put appState
                -- DialogState d -> put appState 
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
              -- DialogState d -> put MenuState 

showLeaderboard :: EventM Name AppState ()
showLeaderboard = do
                    appState <- get
                    case appState of
                      MenuState -> do
                                    r <- liftIO getRecords
                                    put (LeaderboardState (Leaderboard {records = r, startIdx = 0}))
                      GameState g -> put (GameState g)
                      LeaderboardState l -> put (LeaderboardState l)
                      -- DialogState d -> put appState