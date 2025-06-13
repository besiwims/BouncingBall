{-# LANGUAGE RecordWildCards #-}
module Animation.Game
  ( handleInput
  , stepGame
  , drawGame
  ) where

import Control.Monad.Reader        (runReaderT)
import Data.List                   (partition)

import Graphics.Gloss
  ( pictures, translate, rotate, color, polygon, circleSolid, Picture )
import Graphics.Gloss.Data.Color   (black, green, white)
import Graphics.Gloss.Interface.IO.Game
  ( Event(EventKey)
  , Key(Char, SpecialKey)
  , SpecialKey(KeyLeft, KeyRight, KeyUp, KeyDown, KeySpace)
  , KeyState(Down, Up)
  )

import Animation.Environment
  ( Environment
  , frameDimension
  , framePerSecond
  , shooterSpeed
  , bulletSpeed
  , bulletRadius
  , shooterRadius
  , ballRadius
  )
import Animation.State
  ( GameState
  , Shooter(..)
  , Bullet(..)
  , State
  , pos
  )
import Animation.Draw               (drawBalls, stepBalls)

-- | Convert degrees to radians
degToRad :: Floating a => a -> a
degToRad d = d * pi / 180

-- | Handle raw key events
handleInput
  :: Environment -> Event -> GameState -> IO GameState
handleInput env evt (balls, shp@(Shooter p ang vel), blts) =
  case evt of

    EventKey (Char ' ') Down _ _ -> shootFromTip
    EventKey (SpecialKey KeySpace) Down _ _ -> shootFromTip

    EventKey (SpecialKey KeyLeft)  Down _ _ ->
      pure (balls, shp { shooterDir = ang - 10 }, blts)
    EventKey (SpecialKey KeyRight) Down _ _ ->
      pure (balls, shp { shooterDir = ang + 10 }, blts)

    EventKey (SpecialKey KeyUp)   Down _ _ ->
      let v' = thrust  1 in pure (balls, shp { shooterVel = v' }, blts)
    EventKey (SpecialKey KeyDown) Down _ _ ->
      let v' = thrust (-1) in pure (balls, shp { shooterVel = v' }, blts)

    EventKey (SpecialKey KeyUp)   Up _ _ ->
      pure (balls, shp { shooterVel = (0,0) }, blts)
    EventKey (SpecialKey KeyDown) Up _ _ ->
      pure (balls, shp { shooterVel = (0,0) }, blts)

    _ -> pure (balls, shp, blts)
  where
    shootFromTip =
      let speed    = bulletSpeed env
          angleRad = degToRad ang
          (x0, y0) = p
          r        = shooterRadius env
          -- tip is at (2*r, 0) after rotation
          tipX     = x0 + cos angleRad * r * 2
          tipY     = y0 + sin angleRad * r * 2
          velB     = (cos angleRad * speed, sin angleRad * speed)
      in  pure (balls, shp, Bullet (tipX, tipY) velB : blts)

    -- thrust vector in facing dir
    thrust sign =
      let angleRad = degToRad ang
          sp = shooterSpeed env / fromIntegral (framePerSecond env)
      in (sign * cos angleRad * sp, sign * sin angleRad * sp)

-- | Advance everything each frame
stepGame
  :: Environment
  -> Float          -- ^ delta time (seconds)
  -> GameState
  -> IO GameState
stepGame env dt (balls, Shooter (sx,sy) ang (vx,vy), blts) = do
  -- Move balls
  balls' <- runReaderT (stepBalls dt balls) env

  -- Move shooter by its current velocity
  let newShooter = Shooter
        (sx + vx*dt, sy + vy*dt)
         ang
        (vx, vy)
 

      -- Move bullets
      movedBs = [ b { bulletPos = moveBullet dt b } | b <- blts ]

      -- Cull out-of-bounds bullets
      inBounds (Bullet (x,y) _) =
        let (w,h) = frameDimension env
        in  x >= 0 && x <= fromIntegral w
         && y >= 0 && y <= fromIntegral h

      bulletsIn = filter inBounds movedBs

      -- Handle bullet↔ball collisions
      (survivingBalls, survivingBullets) =
        foldl (bulletBallCollide env) (balls', []) bulletsIn

      -- Remove balls hit by the shooter itself
      finalBalls =
        filter (not . shooterHits env newShooter) survivingBalls

  return (finalBalls, newShooter, survivingBullets)
  
-- | Move a bullet by its velocity
moveBullet :: Float -> Bullet -> (Float,Float)
moveBullet dt (Bullet (x,y) (vx,vy)) =
  (x + vx * dt, y + vy * dt)

-- | Remove a bullet if it hit a ball
bulletBallCollide
  :: Environment
  -> ([State],[Bullet])
  -> Bullet
  -> ([State],[Bullet])
bulletBallCollide env (bs,bts) b@(Bullet bp _) =
  let (hits, misses) = partition (\st -> collides env bp (pos st)) bs
  in  if null hits then (bs, bts ++ [b]) else (misses, bts)

-- | True if the shooter overlaps a ball
shooterHits :: Environment -> Shooter -> State -> Bool
shooterHits env (Shooter (sx,sy) _ _) st =
  let (bx,by) = pos st
      d        = sqrt ((bx - sx)^2 + (by - sy)^2)
  in  d < (ballRadius env + shooterRadius env)

-- | True if a bullet overlaps a ball
collides
  :: Environment
  -> (Float,Float)  -- ^ bullet pos
  -> (Float,Float)  -- ^ ball pos
  -> Bool
collides env (x1,y1) (x2,y2) =
  let d   = sqrt ((x1 - x2)^2 + (y1 - y2)^2)
      rad = ballRadius env + bulletRadius env
  in  d < rad

-- | Render everything each frame
drawGame :: Environment -> GameState -> IO Picture
drawGame env (balls, shooter, bullets) = do
  ballPics   <- runReaderT (drawBalls balls) env
  let shooterPic = drawShooter env shooter
      bulletPics = map (drawBullet env) bullets
  return (pictures (ballPics ++ shooterPic : bulletPics))

-- | Shooter is a little green triangle
drawShooter :: Environment -> Shooter -> Picture
drawShooter env (Shooter (x,y) ang _) =
  translate x y $ rotate ang $ color green $
    polygon [(-r, -r), (2*r, 0), (-r, r)]
  where r = shooterRadius env

-- | Bullets are small white circles
drawBullet :: Environment -> Bullet -> Picture
drawBullet env (Bullet (x,y) _) =
  translate x y $ color white $ circleSolid (bulletRadius env)
