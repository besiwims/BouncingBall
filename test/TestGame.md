
# TestGame.hs – Property-Based Test Tutorial

## Table of Contents

1. [Overview](#overview)
2. [Tested Properties](#tested-properties)
3. [Code Example](#code-example)
4. [Glossary](#glossary)

---

## Overview

Tests the input handling logic in `Animation.Game`, ensuring that shooter direction changes in response to key events.

---

## Tested Properties

| Property Name                | Description                                             |
|----------------------------- |--------------------------------------------------------|
| `prop_handleInput_rotation`  | Left/right input events change the shooter’s direction.|

---

## Code Example

```haskell
prop_handleInput_rotation :: Environment -> Shooter -> [State] -> [Bullet] -> Property
prop_handleInput_rotation env shooter balls bullets =
  ioProperty $ do
    let gs = (balls, shooter, bullets)
    (bs, Shooter _ dirL _, blts) <- handleInput env (EventKey (SpecialKey KeyLeft) Down undefined undefined) gs
    (bs2, Shooter _ dirR _, blts2) <- handleInput env (EventKey (SpecialKey KeyRight) Down undefined undefined) gs
    return $ dirL /= dirR
```

---

## Glossary

- **handleInput**: Processes user input events in the game.
- **Shooter**: The player’s controlled entity.
- **Direction**: The angle at which the shooter is facing.

---
