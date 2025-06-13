
# TestEnvironment.hs – Property-Based Test Tutorial

## Table of Contents

1. [Overview](#overview)
2. [Tested Properties](#tested-properties)
3. [Code Example](#code-example)
4. [Glossary](#glossary)

---

## Overview

This test module checks invariants for the `Animation.Environment` module using property-based testing with QuickCheck.  
The tests validate the relationship between window dimensions and bounds, ensuring the environment configuration is always correct.

---

## Tested Properties

| Property Name           | Description                                               |
|------------------------ |----------------------------------------------------------|
| `prop_wBoundCorrect`    | The value of `wBound` is always half the window width.   |
| `prop_xBoundWithin`     | The value of `xBound` never exceeds half the window size.|

---

## Code Example

```haskell
prop_wBoundCorrect :: Environment -> Bool
prop_wBoundCorrect env =
  wBound env == fromIntegral (fst (frameDimension env)) / 2

prop_xBoundWithin :: Environment -> Bool
prop_xBoundWithin env =
  xBound env <= wBound env
```

---

## Glossary

- **Property-based testing**: A testing style that checks the truth of general statements (properties) about your code using random input values.
- **QuickCheck**: The Haskell library used for property-based testing.
- **wBound/xBound**: Functions that calculate the boundaries within which balls can exist.
- **Environment**: A record of global simulation settings.

---
