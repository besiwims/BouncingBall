
# Animation.Draw Tutorial

## Table of Contents

1. [Overview](#overview)
2. [Key Functions](#key-functions)
3. [Diagrams](#diagrams)
4. [Glossary](#glossary)

---

## Overview

Responsible for all **drawing operations**: window setup and rendering all balls. Handles converting game state into graphical representations using Gloss.

---

## Key Functions

| Function      | Purpose                                            |
|---------------|----------------------------------------------------|
| `window`      | Build the Gloss window from Environment.           |
| `drawBalls`   | Return pictures for all balls in the current state.|
| `drawBall`    | Render a single ball as a colored circle.          |
| `stepBalls`   | Move all balls, handling wall collisions.          |
| `stepOne`     | Move one ball and check for wall bounce/collision. |

#### Example (Ball movement logic):

```haskell
stepOne env dt st@State{ pos = (x,y), vel = (vx,vy), ... } = ...
-- Moves and bounces the ball if it hits a wall.
```

---

## Diagrams

**Ball Drawing**

```
+-------------------+
| Ball State        |
|-------------------|
| pos = (x, y)      |
| color             |
+-------------------+
        ↓
[drawBall]
        ↓
(circle at (x,y) of radius r)
```

**Wall Bounce**

```
If ball reaches window edge:
  Reverse direction, increase velocity (up to max)
```

---

## Glossary

- **Picture**: Gloss representation of a drawable object.
- **translate**: Moves drawing position.
- **color/circleSolid**: Draws filled circle with color.

---
