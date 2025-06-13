
# TestInput.hs – Property-Based Test Tutorial

## Table of Contents

1. [Overview](#overview)
2. [Tested Properties](#tested-properties)
3. [Code Example](#code-example)
4. [Glossary](#glossary)

---

## Overview

Tests the pure validation logic extracted from the input-prompting logic in `Animation.Input`. Ensures that ball sizes are accepted only if within valid ranges.

---

## Tested Properties

| Property Name              | Description                                            |
|--------------------------- |-------------------------------------------------------|
| `prop_validBallSize_inRange`| Only ball sizes in `[1, maxR]` are accepted as valid.|

---

## Code Example

```haskell
validBallSize :: Int -> Int -> Bool
validBallSize n maxR = n >= 1 && n <= maxR

prop_validBallSize_inRange :: Int -> Positive Int -> Property
prop_validBallSize_inRange n (Positive maxR) =
  (n >= 1 && n <= maxR) ==> validBallSize n maxR
```

---

## Glossary

- **Validation**: Checking that an input satisfies certain constraints.
- **Property**: A predicate that should hold for a function under test.
- **maxR**: The maximum allowed ball radius.

---
