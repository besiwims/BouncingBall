{-# LANGUAGE ScopedTypeVariables #-}

module TestGame where

import Test.QuickCheck
import Animation.Environment
import Animation.State
import Animation.Game
import Graphics.Gloss.Interface.IO.Game (Event(..), Key(..), SpecialKey(..), KeyState(..))

instance Arbitrary Shooter where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    dir <- arbitrary
    vx <- arbitrary
    vy <- arbitrary
    return $ Shooter (x, y) dir (vx, vy)

instance Arbitrary Environment where
  arbitrary = return getDefaultEnv

instance Arbitrary State where
  arbitrary = do
    i <- arbitrary
    x <- choose (-1000, 1000)
    y <- choose (-1000, 1000)
    vx <- choose (-50, 50)
    vy <- choose (-50, 50)
    dx <- elements [-1, 1]
    dy <- elements [-1, 1]
    vix <- choose (0, 10)
    viy <- choose (0, 10)
    mvx <- choose (1, 500)
    mvy <- choose (1, 500)
    let col = undefined  -- Or use a default Gloss color if needed
    return $ State i (x,y) (vx,vy) (dx,dy) (vix,viy) (mvx,mvy) col

instance Arbitrary Bullet where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    vx <- arbitrary
    vy <- arbitrary
    return $ Bullet (x, y) (vx, vy)

prop_handleInput_rotation :: Environment -> Shooter -> [State] -> [Bullet] -> Property
prop_handleInput_rotation env shooter balls bullets =
  ioProperty $ do
    let gs = (balls, shooter, bullets)
    (bs, Shooter _ dirL _, blts) <- handleInput env (EventKey (SpecialKey KeyLeft) Down undefined undefined) gs
    (bs2, Shooter _ dirR _, blts2) <- handleInput env (EventKey (SpecialKey KeyRight) Down undefined undefined) gs
    return $ dirL /= dirR

main :: IO ()
main = quickCheck prop_handleInput_rotation
