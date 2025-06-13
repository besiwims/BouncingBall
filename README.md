# Coxygen Global Haskell Plutus

# Marathon

## BouncingBall Game

Marathon—as opposed to what I call a Hackathon—is a small app created in order to apply skills learned in a chapter. It must be a little working mock app for real world applications.

It must be working, have tests, and tutorials so that others can use it as a learning tool, and so the Marathon developer can use it for interviews or job applications. It must be extendable to MVP and eventually can be deployed in production for commercial purposes.

**Why Marathons?**
I have been invited to Hackathons on Haskell Plutus but did not have the background to compete with other developers who had done Haskell for years, and I lost. It alienated me. A Marathon, like common athletics marathons, is a team/group coding exercise as opposed to uneven or unfair competition. All Coxygen Haskell Marathon participants must be rewarded, and this helps prepare them for Hackathons.

### Authors

* Ariady Putra ([credly](https://www.credly.com/users/ariady-putra))
* Bernard Sibanda ([github](https://github.com/besiwims/BouncingBall))

A fun and extensible bouncing ball and shooter game written in Haskell, using the [Gloss](https://hackage.haskell.org/package/gloss) library for graphics and animation.

---

## Features

* Multiple bouncing balls with random motion and wall collisions
* Keyboard-controlled shooter that can move, rotate, and fire bullets
* Customizable environment: window size, ball count, ball speed, etc.
* Optional trajectory logging

---

## Directory Structure

```
BouncingBall/
│
├── src/
│   ├── Animation/
│   │   ├── Animation_Draw.md           # Docs for Draw.hs
│   │   ├── Animation_Environment.md    # Docs for Environment.hs
│   │   ├── Animation_Game.md           # Docs for Game.hs
│   │   ├── Animation_Init.md           # Docs for Init.hs
│   │   ├── Animation_Input.md          # Docs for Input.hs
│   │   ├── Animation_State.md          # Docs for State.hs
│   │   ├── Draw.hs
│   │   ├── Environment.hs
│   │   ├── Game.hs
│   │   ├── Init.hs
│   │   ├── Input.hs
│   │   └── State.hs
│   ├── Main.hs
│   └── Main.md                         # Docs for Main.hs
│
├── test/
│   ├── AllTests.hs                     # Main test runner (entry point)
│   ├── TestDraw.hs
│   ├── TestDraw.md
│   ├── TestEnvironment.hs
│   ├── TestEnvironment.md
│   ├── TestGame.hs
│   ├── TestGame.md
│   ├── TestInit.hs
│   ├── TestInit.md
│   ├── TestInput.hs
│   ├── TestInput.md
│   ├── TestState.hs
│   └── TestState.md
│
├── ball_log.txt                        # Gameplay logs
├── board and balls.png                 # Game assets/screenshot
├── BouncingBall.cabal                  # Cabal build config
├── BouncingBall.zip                    # (Optional) compressed archive
├── cabal.project.local                 # Local cabal project settings
├── CHANGELOG.md                        # Project changelog
├── log.txt                             # Log file
├── README.md                           # Project documentation (this file)
```

---

## Prerequisites

* [GHC (Glasgow Haskell Compiler)](https://www.haskell.org/ghc/) >= 8.0
* [Cabal](https://www.haskell.org/cabal/) >= 2.4
* [Gloss](https://hackage.haskell.org/package/gloss) package
* [BouncingBalls](https://github.com/besiwims/BouncingBall)

---

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

---

## Running the Game

* **From Cabal:**

  ```bash
  cabal run
  ```

* **Or directly with GHC:**

  ```bash
  cd src
  ghc -isrc Main.hs -o bouncingball
  ./bouncingball
  ```

* **Customize your session:**
  At startup, the game will prompt you to set window size, ball size, speed, count, and other parameters.

---

## Running the Tests

To run all tests:

```bash
cabal test
# Or manually:
cd test
ghc -itest -isrc AllTests.hs -o runtests
./runtests
```

---

## Controls

* **Arrow keys (`←`, `→`)**: Move/rotate the shooter
* **Space**: Shoot a bullet
* **Q** or window close: Exit the game

---

## Extending

All major logic is organized in modules within `src/Animation/`.
See the tutorials for each module to understand or extend each component!

---

## I am inviting Haskell Students to use this to add these features:

* sounds: bullets fired, exploding balls hit by bullets, stage and levels changing
* dashboard with counters and scores
* enemies and hurdles
* props changes
* distributed peer to peer game
* add hydra technologies
* ada incentives and betting
* blockchain Cardano features
* etc… move from learning to earning ADA!

---

---

