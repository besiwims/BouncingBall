
# Animation.Init Tutorial

## Table of Contents

1. [Overview](#overview)
2. [Key Functions](#key-functions)
3. [Diagrams](#diagrams)
4. [Glossary](#glossary)

---

## Overview

Handles **initialization** for the entire game:  
Creates `Environment`, all initial ball states, shooter, and bullets. Ensures all randomized and user-configurable elements are set before the game starts.

---

## Key Functions

| Function        | Purpose                                           |
|-----------------|---------------------------------------------------|
| `initEnv`       | Build environment by asking user for parameters.  |
| `initBalls`     | Randomly generate starting states for each ball.  |
| `initShooter`   | Place shooter at window center.                   |
| `initBullets`   | Initialize empty bullet list.                     |
| `initGame`      | Combines all above for a complete initial state.  |

#### Example

- **initEnv** gets window/ball config and logging.
- **initBalls** chooses random positions and velocities for each ball, colors, etc.

---

## Diagrams

```
initGame
   ↓
[initEnv] ---+
             |
     +-------+-------+
     |               |
[initBalls]    [initShooter]  [initBullets]
     |               |               |
     +-------+-------+---------------+
             |
         [GameState]
```

---

## Glossary

- **GameState**: Tuple of all dynamic objects (balls, shooter, bullets).
- **RandomRIO**: Random number generator in IO.
- **First Run**: Initialization steps taken only when log file doesn't exist.

---
