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


## Installation and Usage
Use the following cabal command to install packages and run Space Invaders:
```
$ cabal update
$ cabal run spaceinvaders
```

## Testing
Tests are located in the `test` Folder.
To run tests:
```
$ cabal run spaceinvaders-test
```

## Reference
We referred this github repo [BrickInvaders](https://github.com/svbo/BrickInvaders).

## New Features

### Dependency Updates

BrickInvaders has been updated to support the latest versions of core dependencies. Notably, the codebase has been adapted to accommodate changes in `brick` from 0.7x to 2.1.1 and `vty` from 5.x to 6.1.

### Advanced Shooting Mechanism

Players can now fire multiple shots simultaneously, introducing a more dynamic and engaging gameplay experience. Additionally, a new logic for level upgrading has been implemented, providing a rewarding progression system.

### Vertical Movement Capability

Navigate your spaceship not only horizontally but also vertically. This includes sophisticated collision handling with enemies and walls, adding depth to the strategic aspects of the game.

### Enhanced User Interfaces

Enjoy a more polished gaming experience with improved user interfaces, including a start menu and an enhanced in-game interface for better navigation and interaction.

### SQLite Leaderboard Integration

BrickInvaders now features an SQLite-enhanced leaderboard, showcasing score rankings along with additional information such as time and gamer names. Track your progress and compete with others for the top spot!

### Diverse Enemy Types

Encounter new types of enemies, each with unique behaviors and shooting patterns. The game's logic has been expanded to handle the variety in enemy behavior, providing a richer and more challenging gaming environment.

### Visual Damage Indication

Enemies that can withstand multiple shots now feature a visual representation of injury when hit. Witness the evolution of enemy appearance as they sustain damage, providing players with a clear visual cue of their progress in defeating tougher adversaries.

### Special Effects

Experience new visual effects with new shooting effects for both the spaceship and enemies. Additionally, enjoy immersive breaking effects for walls, enhancing the overall visual appeal of the game.

### Redesigned Spaceship and Enemies

Enjoy a fresh look for both the spaceship and enemies, contributing to an updated and visually appealing gaming atmosphere.

### New Unit Tests

The project includes a NEW comprehensive suite of unit tests to ensure the stability and correctness of the dependency migration and new features.
