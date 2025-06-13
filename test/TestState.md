
# TestState.hs – Property-Based Test Tutorial

## Table of Contents

1. [Overview](#overview)
2. [Tested Properties](#tested-properties)
3. [Code Example](#code-example)
4. [Glossary](#glossary)

---

## Overview

Tests the construction of the `State` data type from tuples, verifying field consistency.

---

## Tested Properties

| Property Name                  | Description                                    |
|------------------------------- |----------------------------------------------- |
| `prop_initState_tupleConsistent`| The State fields match the tuple’s values.    |

---

## Code Example

```haskell
prop_initState_tupleConsistent
  :: (Int, (Float,Float), (Float,Float), (Float,Float), (Float,Float), (Float,Float), Color)
  -> Bool
prop_initState_tupleConsistent tuple =
  let st = initState tuple
      (i, p, v, d, vI, mV, c) = tuple
  in ballID st == i && pos st == p && vel st == v
```

---

## Glossary

- **State**: Data type for a single ball’s properties.
- **initState**: Function that creates a `State` from a tuple.
- **Tuple**: A fixed-length group of values, used here for initialization.

---
