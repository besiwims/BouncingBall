{-# LANGUAGE RecordWildCards #-}
module Animation.Draw
  ( window
  , drawBalls
  , stepBalls
  ) where

import Control.Monad.Reader      (Reader, ReaderT, ask)
import Graphics.Gloss 
  ( Display(InWindow)
  , Picture
  , translate
  , circleSolid
  , color
  )
import Graphics.Gloss.Data.Color (Color)

import Animation.Environment
  ( Environment(..)
  , frameDimension
  , ballRadius
  , xBound
  , yBound
  )
import Animation.State           (State(..))

-- | Build the Gloss window from the environment
window :: Reader Environment Display
window = do
  Environment{ frameDimension = (w,h) } <- ask
  return $ InWindow "BouncingBall" (w,h) (100,100)

-- | Draw all balls
drawBalls :: [State] -> ReaderT Environment IO [Picture]
drawBalls sts = do
  env <- ask
  return $ map (drawBall env) sts

-- | Draw one ball as a solid colored circle
drawBall :: Environment -> State -> Picture
drawBall env State{ pos = (x,y), ballColor = col } =
  translate x y $ color col $ circleSolid (ballRadius env)

-- | Step all balls forward by dt, bouncing off walls
stepBalls :: Float -> [State] -> ReaderT Environment IO [State]
stepBalls dt sts = do
  env <- ask
  return $ map (stepOne env dt) sts

-- | Advance one ball by dt, flipping direction on wall collisions
stepOne :: Environment -> Float -> State -> State
stepOne env@Environment{ ballRadius = r } dt
        st@State{ pos = (x,y)
                , vel = (vx,vy)
                , dir = (dx,dy)
                , velInc = (vix,viy)
                , maxVel = (mvx,mvy)
                } =
  let
    -- bounds in centered coords
    xMax = xBound env
    yMax = yBound env

    -- Bounce logic for X axis
    (dx', vx') =
      if x + dx * vx * dt >  xMax
      || x + dx * vx * dt < -xMax
        then (-dx, min (abs (dx * vx) + vix) mvx)
        else (dx, abs (dx * vx))

    -- Bounce logic for Y axis
    (dy', vy') =
      if y + dy * vy * dt >  yMax
      || y + dy * vy * dt < -yMax
        then (-dy, min (abs (dy * vy) + viy) mvy)
        else (dy, abs (dy * vy))

    -- New actual velocities
    vx'' = dx' * vx'
    vy'' = dy' * vy'

    -- New positions
    x' = x + vx'' * dt
    y' = y + vy'' * dt

  in st { pos = (x', y')
        , vel = (vx'', vy'')
        , dir = (dx', dy')
        }
