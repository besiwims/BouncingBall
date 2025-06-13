
# Animation.Input Tutorial

## Table of Contents

1. [Overview](#overview)
2. [Functions](#functions)
3. [Diagrams](#diagrams)
4. [Glossary](#glossary)

---

## Overview

Handles **all user input prompts** for game parameters (window size, ball count, etc.) at startup.  
Includes validation logic for each input and interactive defaults.

---

## Functions

| Function        | Purpose                             |
|-----------------|-------------------------------------|
| `getFrameW`     | Prompt for window width             |
| `getFrameH`     | Prompt for window height            |
| `getBallSize`   | Prompt for ball radius              |
| `getBallInitVel`| Prompt for initial ball speed       |
| `getMaxVelocity`| Prompt for max ball speed           |
| `getWallCharges`| Prompt for wall bounce increment    |
| `getFileLogging`| Prompt to enable/disable logging    |
| `getBallCount`  | Prompt for number of balls          |

All prompts are interactive, with validation and defaults.

#### Example Input Flow

```
Width [min.250, default 1500]: 1200
Height [min.250, default 750]: 600
Ball radius [1..60, default 5]: 8
...
Enable logging? [Y/N, default N]: Y
How many balls [1..400, default 400]: 
```

---

## Diagrams

```
User  →  [Prompt] → [Validation] → [Value in Environment]
```

---

## Glossary

- **Prompt**: A message to the user requesting a value.
- **Validation**: Ensures entered values are within acceptable range.
- **Default**: Value used if the user presses enter with no input.

---
