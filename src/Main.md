
# Main.hs Tutorial

## Table of Contents

1. [Overview](#overview)
2. [Function Flow](#function-flow)
3. [Glossary](#glossary)

---

## Overview

**Entrypoint** of the application. Sets up buffering, gets environment and state, and starts the main Gloss game loop.

---

## Function Flow

1. **hSetBuffering**: Ensures instant terminal output.
2. **initGame**: Initializes environment and starting state.
3. **playIO**: Main Gloss loop — draws frames and responds to events.

```haskell
main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  (env, initialState) <- initGame =<< getArgs
  let (w, h)  = frameDimension env
      display = InWindow "BouncingBall" (w, h) (100, 100)
      fps     = framePerSecond env
  playIO display black fps initialState
    (drawGame env) (handleInput env) (stepGame env)
```

---

## Glossary

- **playIO**: Gloss’s IO-based main loop for rendering and handling input.
- **InWindow**: Gloss window mode (used for desktop-style apps).
- **initGame**: Aggregates all initial state setup for the game.

---
