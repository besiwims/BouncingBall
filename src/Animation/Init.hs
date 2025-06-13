{-# LANGUAGE RecordWildCards #-}
module Animation.Init
  ( initEnv
  , initBalls
  , initShooter
  , initBullets
  , initGame
  ) where

import Control.Monad.Reader    (ReaderT, ask, runReaderT)
import Control.Monad.IO.Class  (liftIO)
import Control.Monad           (replicateM, unless)
import Data.List               (zipWith7)
import System.Environment      (getArgs)
import System.Directory        (doesFileExist)
import System.IO               (writeFile)
import System.Random           (randomRIO)
import Animation.Environment ( Environment(..), getDefaultEnv, xBound, yBound )

import Graphics.Gloss.Data.Color (makeColor)

import Animation.Environment   ( Environment(..)
                                , getDefaultEnv
                                , xBound
                                , yBound
                                )
import Animation.Input         ( getFrameW
                                , getFrameH
                                , getBallSize
                                , getBallInitVel
                                , getMaxVelocity
                                , getWallCharges
                                , getFileLogging
                                , getBallCount
                                )
import qualified Animation.State as Ball
import Animation.State        ( initState
                              , Shooter(..)
                              , Bullet(..)
                              , GameState
                              )

-- | Prompt the user (or use defaults) to build the Environment record.
initEnv :: [String] -> IO Environment
initEnv _args = do
  let env0 = getDefaultEnv

  -- Window dimensions
  fw <- getFrameW
  fh <- getFrameH

  -- Ball radius: max is 1/10th of the lesser window dimension
  let maxR = min (fw `div` 10) (fh `div` 10)
  r  <- getBallSize maxR

  -- Speeds and charges
  v  <- getBallInitVel
  m  <- getMaxVelocity
  c  <- getWallCharges

  -- Logging toggle
  lg <- getFileLogging

  -- Ensure log file exists (to drive first-run logic)
  let filePath = logTxtPath env0
  exists <- doesFileExist filePath
  unless exists $ writeFile filePath ""

  -- Compute maximum ball‐count based on area
  let wndwSize = fw * fh
      ballArea = ceiling (4 * r * r)
      maxBalls = wndwSize `div` ballArea

  -- Ask “How many balls?”, passing (maxBalls, isFirstRun)
  b  <- getBallCount maxBalls (not exists)

  return env0
    { frameDimension = (fw, fh)
    , ballRadius     = r
    , ballStartV     = v
    , maxBallVel     = m
    , ballCharge     = c
    , saveLogTxt     = lg
    , ballCounts     = b
    }

-- | Initialize all the bouncing-ball States with random positions & velocities.
initBalls :: ReaderT Environment IO [Ball.State]
initBalls = do
  env <- ask
  let c      = ballCounts env
      v0     = ballStartV env
      r      = ballRadius env
      xB     = xBound env
      yB     = yBound env
      vInc   = (ballCharge env, ballCharge env)
      mV     = (maxBallVel env,    maxBallVel env)
      colors = cycle
        [ makeColor 1 0 0 1  -- red
        , makeColor 0 1 0 1  -- green
        , makeColor 0 0 1 1  -- blue
        ]

  -- Random positions inside the centered window
  positions  <- replicateM c $ do
    x <- liftIO $ randomRIO (-xBound env, xBound env)
    y <- liftIO $ randomRIO (-yBound env, yBound env)
    return (x, y)

  -- Random velocities in both axes
  velocities <- replicateM c $ do
    vx <- liftIO $ randomRIO (-v0, v0)
    vy <- liftIO $ randomRIO (-v0, v0)
    return (vx, vy)

  -- Random direction multipliers
  dirs       <- replicateM c $ do
    dx <- liftIO $ randomRIO (-1, 1)
    dy <- liftIO $ randomRIO (-1, 1)
    return (dx, dy)

  -- Zip all seven parameters properly into each State
  return $
    zipWith7
      (\i p vel dir vI mV' col ->
          Ball.initState (i, p, vel, dir, vI, mV', col))
      [1 .. c]
      positions
      velocities
      dirs
      (repeat vInc)
      (repeat mV)
      colors

-- | Place the shooter at the Gloss origin (center), pointing right (0°).
initShooter :: ReaderT Environment IO Shooter
initShooter =
  return $ Shooter
    { shooterPos = (0,0)
    , shooterDir = 0
    , shooterVel = (0,0)
    }

-- | No bullets at startup.
initBullets :: ReaderT Environment IO [Bullet]
initBullets = return []

-- | Initialize everything: the Environment plus the full GameState.
initGame :: [String] -> IO (Environment, GameState)
initGame args = do
  env       <- initEnv args
  gameState <- runReaderT
    ((,,) <$> initBalls <*> initShooter <*> initBullets)
    env
  return (env, gameState)
