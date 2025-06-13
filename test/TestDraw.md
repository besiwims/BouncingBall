
# TestDraw.hs – Property-Based Test Tutorial

## Table of Contents

1. [Overview](#overview)
2. [Tested Properties](#tested-properties)
3. [Code Example](#code-example)
4. [Glossary](#glossary)

---

## Overview

Tests the ball-stepping logic in `Animation.Draw`, ensuring that balls never leave their allowed area, regardless of movement or direction.

---

## Tested Properties

| Property Name                   | Description                                                      |
|----------------------------------|-----------------------------------------------------------------|
| `prop_stepOne_ballsStayInBounds` | After stepping, balls always remain within the environment area.|

---

## Code Example

```haskell
prop_stepOne_ballsStayInBounds :: Environment -> State -> Float -> Bool
prop_stepOne_ballsStayInBounds env st dt =
  let st' = stepOne env dt st
      (x, y) = pos st'
      xB = xBound env
      yB = yBound env
  in x >= -xB && x <= xB && y >= -yB && y <= yB
```

---

## Glossary

- **stepOne**: Function that moves a single ball and applies bounce logic.
- **Bounds**: The area within which objects are allowed to move.
- **dt**: Delta time; the time step for simulation.

---
