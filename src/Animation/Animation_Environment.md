
# Animation.Environment Tutorial

## Table of Contents

1. [Overview](#overview)
2. [Main Types](#main-types)
3. [Key Functions](#key-functions)
4. [Diagrams](#diagrams)
5. [Glossary](#glossary)

---

## Overview

Defines the **Environment** type, which holds all the configuration for the simulation—window size, physics constants, shooter, ball, and bullet settings.  
It also provides utilities for bounds calculations and sensible default settings.

---

## Main Types

### `Environment`

```haskell
data Environment = Environment
  { framePerSecond :: Int
  , frameDimension :: (Int, Int)
  , ballRadius     :: Float
  , ballStartV     :: Float
  , ballCharge     :: Float
  , maxBallVel     :: Float
  , ballCounts     :: Int
  , saveLogTxt     :: Bool
  , logTxtPath     :: String
  , shooterSpeed   :: Float
  , shooterRadius  :: Float
  , bulletSpeed    :: Float
  , bulletRadius   :: Float
  }
```

- **framePerSecond**: How many frames per second (FPS) the simulation runs.
- **frameDimension**: Window width and height (pixels).
- **ballRadius**, **ballStartV**, **ballCharge**, **maxBallVel**, **ballCounts**: Ball properties.
- **saveLogTxt**, **logTxtPath**: Trajectory logging options.
- **shooterSpeed**, **shooterRadius**: Shooter movement and collision.
- **bulletSpeed**, **bulletRadius**: Bullet movement and collision.

---

## Key Functions

| Function          | Purpose                                                |
|-------------------|--------------------------------------------------------|
| `getDefaultEnv`   | Returns a default `Environment` for quick testing.     |
| `wBound`, `hBound`| Returns half the window width/height (centered origin).|
| `xBound`, `yBound`| Returns spawn/movement limits, subtracting ball radius.|

Example:

```haskell
wBound env = fromIntegral (fst (frameDimension env)) / 2
xBound env = wBound env - ballRadius env
```

---

## Diagrams

**Environment in Memory**

```
+-------------------------+
|    Environment         |
|------------------------|
| framePerSecond         |
| frameDimension         |
| ballRadius             |
| ...                    |
+------------------------+
```

**Coordinate System**

```
        Y↑
         |
   (-x,y)|    (x,y)
         |----------→ X
   (-x,-y)|  (x,-y)
         |
```

---

## Glossary

- **Environment**: All game parameters, passed to nearly every function.
- **Bound**: The furthest position a ball or bullet can occupy without leaving the window.
- **Shooter**: Player-controlled object.
- **Gloss**: Haskell graphics library for simple 2D animation.

---
