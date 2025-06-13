{-# LANGUAGE ScopedTypeVariables #-}

module TestDraw where

import Test.QuickCheck
import Animation.Environment
import Animation.State
import Animation.Draw

instance Arbitrary Environment where
  arbitrary = do
    w <- choose (300, 3000)
    h <- choose (200, 2000)
    r <- choose (1, 50)
    return $ getDefaultEnv { frameDimension = (w, h), ballRadius = r }

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
    let col = undefined -- Use a default color or skip color checks for property
    return $ State i (x,y) (vx,vy) (dx,dy) (vix,viy) (mvx,mvy) col

prop_stepOne_ballsStayInBounds :: Environment -> State -> Float -> Bool
prop_stepOne_ballsStayInBounds env st dt =
  let st' = stepOne env dt st
      (x, y) = pos st'
      xB = xBound env
      yB = yBound env
  in x >= -xB && x <= xB && y >= -yB && y <= yB

main :: IO ()
main = quickCheck prop_stepOne_ballsStayInBounds
