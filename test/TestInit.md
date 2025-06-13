
# TestInit.hs – Property-Based Test Tutorial

## Table of Contents

1. [Overview](#overview)
2. [Tested Properties](#tested-properties)
3. [Code Example](#code-example)
4. [Glossary](#glossary)

---

## Overview

Tests the initialization logic in `Animation.Init`, making sure the right number of balls are created according to the environment’s configuration.

---

## Tested Properties

| Property Name        | Description                                        |
|--------------------- |---------------------------------------------------|
| `prop_initBalls_count`| Number of initialized balls matches config value. |

---

## Code Example

```haskell
prop_initBalls_count :: Environment -> Property
prop_initBalls_count env =
  ioProperty $ do
    balls <- runReaderT initBalls env
    return $ length balls == ballCounts env
```

---

## Glossary

- **ioProperty**: Allows QuickCheck to test properties that perform IO.
- **initBalls**: Initializes the starting state for all balls.
- **ballCounts**: The number of balls specified in the environment.

---
