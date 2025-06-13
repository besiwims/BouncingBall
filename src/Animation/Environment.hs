module Animation.Environment where

-- | Configuration for the bouncing-ball shooter game
data Environment = Environment
  { framePerSecond :: Int           -- ^ Simulation FPS
  , frameDimension :: (Int, Int)    -- ^ (width, height) in pixels
  , ballRadius     :: Float         -- ^ Radius of each ball
  , ballStartV     :: Float         -- ^ Initial ball speed
  , ballCharge     :: Float         -- ^ Bounce “charge” increment
  , maxBallVel     :: Float         -- ^ Maximum ball speed
  , ballCounts     :: Int           -- ^ Number of balls
  , saveLogTxt     :: Bool          -- ^ Enable trajectory logging
  , logTxtPath     :: String        -- ^ Log file path
  -- Shooter settings
  , shooterSpeed   :: Float         -- ^ Shooter move speed (px/sec)
  , shooterRadius  :: Float         -- ^ Shooter collision radius
  , bulletSpeed    :: Float         -- ^ Bullet speed (px/sec)
  , bulletRadius   :: Float         -- ^ Bullet radius
}deriving (Show)                                                                                                              

-- | Sensible defaults for quick experimentation
getDefaultEnv :: Environment
getDefaultEnv = Environment
  { framePerSecond = 360
  , frameDimension = (1500, 750)
  , ballRadius     = 5
  , ballStartV     = 5
  , ballCharge     = 5
  , maxBallVel     = 200
  , ballCounts     = 10
  , saveLogTxt     = False
  , logTxtPath     = "ball_log.txt"
  , shooterSpeed   = 200
  , shooterRadius  = 10
  , bulletSpeed    = 400
  , bulletRadius   = 3
  }

-- Half‐width and half‐height in centered coords
wBound :: Environment -> Float
wBound env = fromIntegral (fst (frameDimension env)) / 2

hBound :: Environment -> Float
hBound env = fromIntegral (snd (frameDimension env)) / 2

-- Spawn limits (edge minus radius)
xBound :: Environment -> Float
xBound env = wBound env - ballRadius env

yBound :: Environment -> Float
yBound env = hBound env - ballRadius env
