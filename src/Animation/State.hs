{-# LANGUAGE RecordWildCards #-}
module Animation.State
  ( State(..)
  , initState
  , Shooter(..)
  , Bullet(..)
  , GameState
  ) where

import Graphics.Gloss (Color)

-- | 2D vector alias
type XY = (Float, Float)

-- | One bouncing‐ball’s mutable state
data State = State
  { ballID    :: Int       -- ^ unique identifier
  , pos       :: XY        -- ^ (x,y) position
  , vel       :: XY        -- ^ (vx,vy) velocity
  , dir       :: XY        -- ^ direction multipliers (+1 or –1)
  , velInc    :: XY        -- ^ how much to change velocity on a bounce
  , maxVel    :: XY        -- ^ upper bound on speed
  , ballColor :: Color     -- ^ color for rendering
  }
  deriving (Show, Eq)

-- | Construct a State from the 7‐tuple you build in initBalls
initState :: (Int, XY, XY, XY, XY, XY, Color) -> State
initState (bid, p, v, d, vI, mV, col) =
  State bid p v d vI mV col

-- | The shooter can move, rotate, *and* carry a current thrust velocity
data Shooter = Shooter
  { shooterPos :: (Float, Float)
  , shooterDir :: Float
  , shooterVel :: (Float, Float)
  }

-- | A bullet that the shooter fires
data Bullet = Bullet
  { bulletPos :: XY            -- ^ (x,y) position
  , bulletVel :: XY            -- ^ (vx,vy) velocity
  }
  deriving (Show, Eq)

-- | Full game snapshot: balls, shooter, and active bullets
type GameState = ([State], Shooter, [Bullet])
