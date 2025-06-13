
# Animation.Game Tutorial

## Table of Contents

1. [Overview](#overview)
2. [Key Functions](#key-functions)
3. [Diagrams](#diagrams)
4. [Glossary](#glossary)

---

## Overview

Handles all **game logic per frame**:  
Processing user input, advancing state, collision handling, and rendering. This module connects the physics, state updates, and user controls.

---

## Key Functions

| Function      | Purpose                                              |
|---------------|------------------------------------------------------|
| `handleInput` | Responds to keyboard input (move, rotate, shoot)     |
| `stepGame`    | Advances all objects (balls, shooter, bullets)       |
| `drawGame`    | Combines all visuals for the frame                   |
| `drawShooter` | Draws the shooter as a green triangle                |
| `drawBullet`  | Draws bullets as white circles                       |

#### Example

- Arrow keys change shooter direction or thrust.
- Space fires bullets.
- Each frame, moves everything and checks collisions.

---

## Diagrams

**Frame Loop**

```
[Input] ──> [handleInput] ──┐
                            ↓
                       [stepGame]
                            ↓
                       [drawGame]
```

**Shooter/Collision Illustration**

```
    ↑ Y
    |
    |        o (Ball)
    |
    +-----> (Shooter) --o--> (Bullet)
   (0,0)
```

---

## Glossary

- **Event**: Key press or release.
- **Thrust**: Velocity applied in the direction the shooter is facing.
- **Collision**: When a ball and bullet/shooter overlap.
- **stepGame**: Moves all game objects forward one frame and checks for interactions.

---
