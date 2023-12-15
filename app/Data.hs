{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module Data where

import Linear.V2 (V2(..))
import Leaderboard

type Name = ()
type Coord = V2 Int

data Tick = Tick
data Cell
  = CanonCell
  | EmptyCell
  | ShotCell
  | AlienCell
  | UfoCell
  | BlockerCell0
  | BlockerCell1
  | BlockerCell2
  | AlienShotCell
  | NewAlienCell
  | NewAlienHitOnceCell
  deriving (Show, Eq)

data Status = Active | Paused | Lost | Won deriving (Show, Eq)
data Direction = L | R | D deriving (Show, Eq)

data Level = Level
  { lNext      :: Int
  , lAliens    :: [Alien]
  , lSpeed     :: Int
  , lAShotSpeed :: Int
  , lUfoSpeed   :: Int
  , lShots     :: Int
  } deriving (Show)

data AlienType = RegularAlien | NewAlien deriving (Show, Eq)

data Alien = Alien
  { coord     :: Coord
  , hits      :: Int
  , alienType :: AlienType
  } deriving (Show, Eq)

data Ufo = Ufo
  { uCoord :: Coord
  , uHits  :: Int
  } deriving (Show, Eq)

data Blocker = Blocker
  { bCoord  :: Coord
  , bHealth :: Int
  } deriving (Show)

data Game = Game
  { canon      :: Coord
  , status     :: Status
  , lives      :: Int
  , level      :: Level
  , shots      :: [Coord]
  , alienShots :: [Coord]
  , aliens     :: [Alien]
  , ufo        :: [Ufo]
  , ufoCount   :: Int
  , count      :: Int
  , aShotCount :: Int
  , blockers   :: [Blocker]
  , alienDir   :: Direction
  , score      :: Int
  } deriving (Show)

data AppState = MenuState | GameState Game | LeaderboardState Leaderboard

generateGame :: Int -> Int -> Level -> Game
generateGame s li l = Game
  { canon      = V2 10 0
  , status     = Active
  , shots      = []
  , alienShots = []
  , lives      = li
  , level      = l
  , aliens     = lAliens l
  , ufo        = []
  , ufoCount   = 1
  , count      = 1
  , aShotCount = 1
  , blockers   = getBlockers 5 ++ getBlockers 15 ++ getBlockers 25
  , alienDir   = R
  , score      = s
  }

getBlockers :: Int -> [Blocker]
getBlockers x = b1 ++ b2
  where
    b1 = [Blocker (V2 (x + i) 2) 3 | i <- [0..5]]
    b2 = [Blocker (V2 (x + 1 + i) 4) 3 | i <- [0..3]]

createUfo :: [Ufo]
createUfo = [Ufo (V2 0 (height - 1)) 1]

initAppState :: IO AppState
initAppState = return MenuState

initGame :: Game
initGame = generateGame 0 3 (generateLevel 0)

height, width :: Int
height = 15
width = 35

newAlientLocations :: Game -> [Coord]
newAlientLocations g = map coord $ filter (\alien -> alienType alien == NewAlien) $ aliens g

alientLocations :: Game -> [Coord]
alientLocations g = map coord $ aliens g

ufoLocations :: Game -> [Coord]
ufoLocations g = map uCoord $ ufo g

blockerLocations :: Game -> Int -> [Coord]
blockerLocations g h = [bCoord x | x <- blockers g, bHealth x == h]

allBlockerLocations :: Game -> [Coord]
allBlockerLocations g = map bCoord $ blockers g

stopped :: Game -> Bool
stopped g = s == Lost || s == Paused || s == Won
  where
    s = status g

generateLevel :: Int -> Level
generateLevel l = Level
  { lNext      = l + 1
  , lAliens    = createAliensForLevel l
  , lSpeed     = getLevelSpeed l
  , lAShotSpeed = getLevelSpeed l * 4
  , lUfoSpeed   = getLevelSpeed l - 1
  , lShots     = 2 + l
  }

createAliensForLevel :: Int -> [Alien]
createAliensForLevel l =
  let newAliens = createAliens p n 2 14 2 NewAlien
      regularAliens = createAliens (p + zigzagOffset) n 2 15 1 RegularAlien
  in newAliens ++ regularAliens
  where
    n = 5 + l - (l `div` 5 * 5)
    p = 15 - n
    zigzagOffset = 1

createAliens :: Int -> Int -> Int -> Int -> Int -> AlienType -> [Alien]
createAliens f n o y h t = [Alien (V2 (f + x * o) y) h t | x <- [0..n]]

getLevelSpeed :: Int -> Int
getLevelSpeed l
  | l < 2 = 10
  | l < 4 = 9
  | l < 6 = 8
  | l < 8 = 7
  | l < 15 = 6
  | l < 20 = 5
  | otherwise = 4
