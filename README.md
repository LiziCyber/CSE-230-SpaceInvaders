# CSE230 Final Project -- Space Invaders

#### Fall 2023

#### Authors:
- Jiahao Li
- Shanhe Ding
- Ziping Li
- Zengmin Xiao

## Overview
Space Invaders is an exciting space game where you control a spaceship to fight against aliens. The game combines action and strategy, perfect for players who enjoy both challenges and fun. 

It is great for those who love classic space games but also want something new. It's easy to learn but also makes you think and act quickly. Whether you are new to games or have been playing for years, Space Invaders offers fun and excitement.


## Elements
- Spaceship: Players can operate it to move to all four directions (↑←↓→). It has three lives at the beginning. Players can also improve the power of bullets at some points.

- Aliens: They move side by side all the time. As time progresses, they also descend vertically. Different aliens have different number of lives.

- Barriers: Players can utilize strategically placed barriers for cover. However, barriers may break if hit by bullets. 


## Game Rules
- Winning condition: Players need to destroy all aliens to win. The game fails if any aliens reach the ground, or the spaceship has been shot for three times.

- Scoring: Players earn points for each alien they defeat, depending on the type of the alien. Meanwhile, there is a special enemy spaceship in the game -- beating it gives 10 points directly.

- Leaderboard: Players can upload their scores in a leaderboard to compete with other records of the game.

- Difficulty levels: Level grows up while players win the current stage. The power of bullets and number of aliens grows as level grows.


## Instruction
- Move Left: Arrow Left
- Move Right: Arrow Right
- Move Up: Arrow Up
- Move Down: Arrow Down
- Fire: Space
- Leaderboard: `L` 
- Back: `ESC` 
- Pause: `P` 
- Restart: `R` 


## Architecture
The game is structured into five core modules:
- **Data.hs:** Defines game data types and structures.
- **GameHandler.hs:** Handles the game logic and state updates.
- **UI.hs:** Focuses on rendering the game's user interface.
- **Leaderboard.hs:** Focuses on rendering the game's user interface.
- **Main.hs:** Manages the integration of game logic with the user interface.


## Challenges
A notable challenge in our game development surfaced while implementing the interaction mechanics with barriers. These barriers were composed of multiple bricks, each designed to withstand up to three hits before completely disappearing. The complexity arose in visually and functionally differentiating the bricks based on the number of hits they had endured. After a brick was hit once or twice, it needed to not only exhibit a distinct appearance to reflect its damaged state but also maintain its interactive properties correctly. This required a careful blend of graphical changes and game logic adjustments. We had to ensure that each hit visibly altered the brick's appearance in a clear and understandable way for the players, while also maintaining the integrity of the game's physics and collision detection systems. Balancing the visual feedback to players and the underlying game mechanics for these evolving barrier states proved to be a meticulous and challenging aspect of our development process.

Another challenge we met was handling the efficient storage and retrieval of leaderboard records in the SQLite database, particularly in ensuring that the data schema and queries were optimized for quick access and updates. Managing the leaderboard's growing data size while maintaining fast query performance for displaying high scores and implementing pagination effectively, posed a complex task, especially in an offline, resource-constrained environment.


## Installation
Use the following cabal command to install Space Invaders:
```
$ cabal update
$ cabal run spaceinvaders
```


## Usage
Run the executable created after compilation to play Space Invaders:
```
./Main
```

## Testing
Tests are located in the `test` Folder.
To run tests:
```
$ cabal run spaceinvaders-test
```
