# Coxygen Global Haskell Plutus 

# Marathon

## BouncingBall Game 

Marathon as opposed to what I call Hackathon is a small App created in order to apply skills learned on a chapter. It must be a little working mock app
on real world applications.

It must be working, have tests, tutorials so that others can use it as a learning tool and the Marathon developer can use it for interviews in looking for jobs. It must be extendable to MVP and eventually can be deployed in production for commercial purposes.

Why Marathons? I have been invited to Hackathons on Haskell Plutus I did not have background competing with other developers who had done Haskell for past n-years and lost. It alienated me. Marathon like common athletics marathons are teams/group coding exercises as opposed to uneven/unfair competing. All Coxygen Haskell Marathon participants must be rewarded and this helps prepare them for Hackathons.

# Ariady Putra (https://www.credly.com/users/ariady-putra); Bernard Sibanda(https://github.com/besiwims/BouncingBall)

A fun and extensible bouncing ball and shooter game written in Haskell, using the [Gloss](https://hackage.haskell.org/package/gloss) library for graphics and animation.

## Features

- Multiple bouncing balls with random motion and wall collisions
- Keyboard-controlled shooter that can move, rotate, and fire bullets
- Customizable environment: window size, ball count, ball speed, etc.
- Optional trajectory logging

## Directory Structure

```
BouncingBall/
│
├── src/
│   ├── Main.hs
│   └── Animation/
│       ├── Draw.hs
│       ├── Environment.hs
│       ├── Game.hs
│       ├── Init.hs
│       ├── Input.hs
│       └── State.hs
├── BouncingBall.cabal
└── README.md
```

## Prerequisites

- [GHC (Glasgow Haskell Compiler)](https://www.haskell.org/ghc/) >= 8.0
- [Cabal](https://www.haskell.org/cabal/) >= 2.4
- [Gloss](https://hackage.haskell.org/package/gloss) package
- [BouncingBalls](https://github.com/besiwims/BouncingBall)

## Installation

1. **Clone the repository**:
    ```bash
    git clone <your-repo-url>
    cd BouncingBall
    ```

2. **Install dependencies**:
    ```bash
    cabal update
    cabal install --only-dependencies
    ```

3. **Build the project**:
    ```bash
    cabal build
    ```

## Running the Game

- **From Cabal**:
    ```bash
    cabal run
    ```

- **Or directly with GHC**:
    ```bash
    cd src
    ghc -isrc Main.hs -o bouncingball
    ./bouncingball
    ```

- **Customize your session:**  
    At startup, the game will prompt you to set window size, ball size, speed, count, and other parameters.

## Controls

- **Arrow keys <- and -> for rotation : left and right**: Move/rotate the shooter
- **Space for shooting bullets**: Shoot a bullet
- **Q** or window close: Exit the game

## Extending

All major logic is organized in modules within `src/Animation/`.  
See the tutorials for each module in order to understand or extend each component!

## I am inviting Haskell Students to use this to add these features:
- sounds: bullets fired, exploding balls hit by bullets, stage and levels changing 
- dashboard with counters and scores
- enermies and hurdles
- props changes
- distributed peer to peer game
- add hydra technologies
- ada incentives and betting
- blockchain Cardano features
- etc move from learning to earning ADA!
---
