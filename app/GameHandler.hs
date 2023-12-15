module GameHandler where

import Data
import Leaderboard

import Linear.V2 (V2(..), _x, _y)
import Control.Lens ((^.))
import Data.Time (getCurrentTime)

-- | Pause the game
pause :: Game -> Game
pause g = case status g of
  Active -> g { status = Paused }
  Paused -> g { status = Active }
  _ -> g

-- | Restart the game
restart :: Game -> Game
restart _ = generateGame 0 3 (generateLevel 0)

-- | Add new shot from the canon to the game
shoot :: Game -> Game
shoot g
  | stopped g || length s >= lShots l = g
  | otherwise = g { shots = n : s }
  where
    s = shots g
    n = fmap (\(V2 x y) -> V2 x y) canon g
    l = level g

-- | Move the canon left or right
moveHorizontal :: (Int -> Int) -> Game -> Game
moveHorizontal f g
  | stopped g = g
  | otherwise = g { canon = V2 x (canon g ^. _y), lives = newLives }
  where
    oldX = canon g ^. _x
    newLives = if V2 x (canon g ^. _y) `elem` [coord alien | alien <- aliens g] then max 0 (lives g - 1) else lives g
    x = if V2 new_x (canon g ^. _y) `elem` [bCoord blocker | blocker <- blockers g] then oldX else new_x
    new_x = f oldX `mod` width

-- | Move the canon up or down
moveVertical :: (Int -> Int) -> Game -> Game
moveVertical f g
  | stopped g = g
  | otherwise = g { canon = V2 (canon g ^. _x) y, lives = newLives }
  where
    oldY = canon g ^. _y
    newLives = if V2 (canon g ^. _x) y `elem` [coord alien | alien <- aliens g] then max 0 (lives g - 1) else lives g
    y
      | new_y < 0 = 0
      | new_y >= height = height - 1
      | otherwise = if V2 (canon g ^. _x) new_y `elem` [bCoord blocker | blocker <- blockers g] then oldY else new_y
    new_y = f oldY

-- | Change to the next level if all aliens are dead
levelUp :: Game -> IO Game
levelUp g
  | lives g == 0 && status g == Active = do
    currentTime <- getCurrentTime
    let record = LeaderboardRecord currentTime (score g) "Harry"
    insertRecord record
    return $ g { status = Lost }
  | lives g == 0 = return $ g { status = Lost }
  | not (null $ aliens g) = return g
  | n <= 25 = return $ nextLevelGame g
  | otherwise = if status g == Active
    then do
      currentTime <- getCurrentTime
      let record = LeaderboardRecord currentTime (score g) "Harry"
      insertRecord record
      return $ g { status = Won }
    else return $ g { status = Won }
  where
    n = lNext $ level g
    level_n = generateLevel n
    nextLevelGame g' = g' { level = level_n, aliens = lAliens level_n, ufo = [], alienDir = R }

-- | Remove dead aliens and trigger alien movement
handleAliens :: Game -> [Coord] -> [Alien]
handleAliens g s = do
  let a = moveAndKill (aliens g) s
  if count g > 0 then a
  else do
    let a' = map (moveAlien (alienDir g)) a -- move aliens
    moveAndKill a' s -- check for hits again after moving aliens

moveAndKill :: [Alien] -> [Coord] -> [Alien]
moveAndKill a s = [x | x <- a', hits x /= 0]
  where
    a' = map (\(Alien c h t) -> if c `elem` s then Alien c (h - hitReduction t) t else Alien c h t) a

hitReduction :: AlienType -> Int
hitReduction RegularAlien = 1
hitReduction NewAlien = 1  -- Assuming the new alien type takes 2 hits to kill

-- | Move aliens according to the current direction
moveAlien :: Direction -> Alien -> Alien
moveAlien R (Alien c h t) = Alien (V2 (c ^. _x + 1) (c ^. _y)) h t
moveAlien L (Alien c h t) = Alien (V2 (c ^. _x - 1) (c ^. _y)) h t
moveAlien D (Alien c h t) = Alien (V2 (c ^. _x) (c ^. _y - 1)) h t

-- | Update blockers' health or remove them if their health is 0 or if they're in the way of an alien
handleBlocker :: Game -> [Coord] -> [Coord] -> [Blocker]
handleBlocker g s as = do
  let b1 = map (\(Blocker c h) -> if c `elem` s || c `elem` as then Blocker c (h - 1) else Blocker c h) $ blockers g -- check for hits
  let b2 = map (\(Blocker c h) -> if c `elem` alientLocations g then Blocker c 0 else Blocker c h) b1 -- check for aliens
  [x | x <- b2, bHealth x /= 0] -- remove blockers which hit

-- | Update the player's score (1P. for Aliens / 10P. for UFOs)
handleScore :: Game -> [Coord] -> Int
handleScore g s = score g + sum (map alienScore h) + length u * 10
  where
    h = [x | x <- aliens g, coord x `elem` s]
    u = [x | x <- s, x `elem` ufoLocations g]

alienScore :: Alien -> Int
alienScore (Alien _ _ RegularAlien) = 1
alienScore (Alien _ _ NewAlien) = 3  -- Assuming the new alien type gives 3 points

-- | Remove shots that hit something or are out of level bounds
handleShots :: Game -> [Coord] -> [Coord]
handleShots g s = do
  let s1 = [x | x <- s, not (x `elem` alientLocations g || x `elem` allBlockerLocations g || x `elem` ufoLocations g)] -- remove shots which hit
  [x | x <- s1, x ^. _y <= height] -- remove shots which are out

-- | Remove alien shots that hit something or are out of level bounds and add a new alien shot
handleAlienShots :: Game -> [Coord] -> [Coord]
handleAlienShots g s = do
  let s1 = [x | x <- s, not (x == canon g || x `elem` allBlockerLocations g)] -- remove shots which hit
  let s2 = [x | x <- s1, x ^. _y >= 0] -- remove shots which are out
  if aShotCount g > 0 then s2
  else s2 ++ [fmap (\(V2 x y)  -> V2 x (y - 1)) coord (aliens g !! (length (aliens g) `div`2) )] --add new shot

-- | Change alien direction if they get near the level bounds
handleDirection :: Game -> [Alien] -> Direction
handleDirection g a = if count g > 0 then alienDir g
  else do
    let isLeft = any (\(Alien c _ _) -> c ^. _x <= 5) a
    let isRight = any (\(Alien c _ _) -> c ^. _x >= width - 5) a
    case alienDir g of
      D -> if isLeft then R else L
      L -> if isLeft then D else L
      R -> if isRight then D else R

-- | Remove dead UFOs or UFOs that are out of level bounds and add a new UFO when aliens go down
handleUfo :: Game -> [Coord] -> [Ufo]
handleUfo g s = do
  let a = map (\(Ufo c h) -> if c `elem` s then Ufo c (h - 1) else Ufo c h) $ ufo g -- check for hits
  let a1 = [x | x <- a, 0 /= uHits x && width > uCoord x ^. _x] -- remove dead and outside UFO
  if alienDir g == D && null (ufo g) then a1 ++ createUfo -- new UFO when aliens go down and no UFO present
  else if ufoCount g > 0 then a1
  else map (\(Ufo c h) -> Ufo (V2 (c ^. _x + 1) (c ^. _y)) h) a1

-- | Update player's lives if he's hit by an alien and finish the game if aliens are at the bottom
handleLives :: Game -> [Coord] -> [Alien] -> Int
handleLives g ashots as
  | a = 0
  | isCollision = lives g - 1
  | null h = lives g
  | otherwise = lives g - 1
  where
    h = [x | x <- ashots, x == canon g] -- canon is hit
    a = any (\c -> c ^. _y <= 1) (alientLocations g) -- game over when aliens at the bottom
    isCollision = count g == 0 && canon g `elem` [coord alien | alien <- as]

-- | Main game tick counter
nextCount :: Game -> Int
nextCount g = if count g < lSpeed (level g) then count g + 1 else 0

-- | Game tick counter for new alien shot
nextAShotCount :: Game -> Int
nextAShotCount g = if aShotCount g < lAShotSpeed (level g) then aShotCount g + 1 else 0

-- | Game tick counter for UFO movement speed
nextUfoCount :: Game -> Int
nextUfoCount g = if ufoCount g < lUfoSpeed (level g) then ufoCount g + 1 else 0
