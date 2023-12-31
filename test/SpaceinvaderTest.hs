module Main (main) where

import Data
import GameHandler (nextCount, handleLives, handleAliens)

import Linear.V2 (V2(..))
import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit (testCase, (@?=))

-- entry point for tests
main :: IO ()
main = defaultMain tests

-- all tests
tests :: TestTree
tests = testGroup "Tests" [nextCountTest, handleLivesTest, handleAliensTest, handleAliensTestMove]

-- Test for next count function
nextCountTest :: TestTree
nextCountTest = testGroup "Tests for nextCount"
  [ testCase "count 0 < level speed" $
    nextCount (getGameWithCountLevel0 0) @?= 1,

    testCase "count 9 < level speed" $
    nextCount (getGameWithCountLevel0 9) @?= 10,

    testCase "count == level speed" $
    nextCount (getGameWithCountLevel0 10) @?= 0
  ]

-- Test for handleLives function
handleLivesTest :: TestTree
handleLivesTest = testGroup "Tests for handleLives"
  [ testCase "aliens at bottom" $
    handleLives (getGameWithAliens [Alien (V2 2 1) 1 RegularAlien]) [] [] @?= 0,

    testCase "canon is hit" $
    handleLives getGameLevel0 [V2 10 0] [] @?= 2,

    testCase "no Alien Shots" $
    handleLives (getGameWithCountLevel0 0) [] [] @?= 3
  ]

handleAliensTest :: TestTree
handleAliensTest = testGroup "Tests for handleAliens, aliens do not move"
  [ testCase "alien to the right of shot" $
    handleAliens (getGameWithAliens [Alien (V2 2 1) 2 RegularAlien]) [V2 1 1] @?= [Alien (V2 2 1) 2 RegularAlien],

    testCase "alien to the left of shot" $
    handleAliens (getGameWithAliens [Alien (V2 1 1) 2 RegularAlien]) [V2 2 1] @?= [Alien (V2 1 1) 2 RegularAlien],

    testCase "alien above shot" $
    handleAliens (getGameWithAliens [Alien (V2 1 3) 2 RegularAlien]) [V2 1 2] @?= [Alien (V2 1 3) 2 RegularAlien],

    testCase "alien below shot" $
    handleAliens (getGameWithAliens [Alien (V2 1 3) 2 RegularAlien]) [V2 1 4] @?= [Alien (V2 1 3) 2 RegularAlien],

    testCase "alien and shot at the same position" $
    handleAliens (getGameWithAliens [Alien (V2 2 1) 2 RegularAlien]) [V2 2 1] @?= [Alien (V2 2 1) 1 RegularAlien]
  ]

handleAliensTestMove :: TestTree
handleAliensTestMove = testGroup "Tests for handleAliens, aliens do move"
  [ testCase "alien to the right of shot" $
    handleAliens (getGameWithAliensAndCountZero [Alien (V2 2 1) 2 RegularAlien]) [V2 1 1] @?= [Alien (V2 3 1) 2 RegularAlien],

    testCase "alien to the left of shot" $
    handleAliens (getGameWithAliensAndCountZero [Alien (V2 1 1) 2 RegularAlien]) [V2 2 1] @?= [Alien (V2 2 1) 1 RegularAlien], -- shot and alien at the same position!!

    testCase "alien above shot" $
    handleAliens (getGameWithAliensAndCountZero [Alien (V2 1 3) 2 RegularAlien]) [V2 1 2] @?= [Alien (V2 2 3) 2 RegularAlien],

    testCase "alien below shot" $
    handleAliens (getGameWithAliensAndCountZero [Alien (V2 1 3) 2 RegularAlien]) [V2 1 4] @?= [Alien (V2 2 3) 2 RegularAlien],

    testCase "alien and shot at the same position" $
    handleAliens (getGameWithAliensAndCountZero [Alien (V2 2 1) 2 RegularAlien]) [V2 2 1] @?= [Alien (V2 3 1) 1 RegularAlien]
  ]

-- Helper methods to create test data
getGameWithCountLevel0 :: Int -> Game
getGameWithCountLevel0 c = getGameLevel0 { count = c }

getGameWithAliens :: [Alien] -> Game
getGameWithAliens a = getGameLevel0 { aliens = a }

getGameWithAliensAndCountZero :: [Alien] -> Game
getGameWithAliensAndCountZero a = getGameLevel0 { aliens = a, count = 0 }

getGameLevel0 :: Game
getGameLevel0 = generateGame 0 3 (generateLevel 0)
