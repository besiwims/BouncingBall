
# Animation.State Tutorial

## Table of Contents

1. [Overview](#overview)
2. [Types](#types)
3. [Diagrams](#diagrams)
4. [Glossary](#glossary)

---

## Overview

Defines **all mutable state** in the game:  
Balls, shooter, bullets, and the overall game state. Structures used by almost all other modules.

---

## Types

| Name          | Description                                  |
|---------------|----------------------------------------------|
| `State`       | One ball’s state: position, velocity, color. |
| `Shooter`     | The player-controlled shooter (position, etc).|
| `Bullet`      | Bullets shot by the shooter.                 |
| `GameState`   | Tuple of ([State], Shooter, [Bullet])        |

#### Example

```haskell
data State = State
  { ballID    :: Int
  , pos       :: (Float, Float)
  , vel       :: (Float, Float)
  , dir       :: (Float, Float)
  , velInc    :: (Float, Float)
  , maxVel    :: (Float, Float)
  , ballColor :: Color
  }
```

---

## Diagrams

**GameState Structure**

```
+-------------------------------+
|       GameState               |
|-------------------------------|
| [State]   Shooter   [Bullet]  |
+-------------------------------+
```

---

## Glossary

- **State**: A record for a single ball.
- **Shooter**: Player’s ship that can move and rotate.
- **Bullet**: Projectile with position and velocity.
- **initState**: Helper function for easy creation of a State.
- **XY**: Type alias for (Float, Float) coordinates.

---
