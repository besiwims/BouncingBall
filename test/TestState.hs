{-# LANGUAGE ScopedTypeVariables #-}

module TestState   where

import Test.QuickCheck
import Animation.State
import Graphics.Gloss (Color, makeColor)

instance Arbitrary State where
  arbitrary = do
    i <- arbitrary
    x <- arbitrary
    y <- arbitrary
    vx <- arbitrary
    vy <- arbitrary
    dx <- arbitrary
    dy <- arbitrary
    vix <- arbitrary
    viy <- arbitrary
    mvx <- arbitrary
    mvy <- arbitrary
    let col = undefined
    return $ State i (x, y) (vx, vy) (dx, dy) (vix, viy) (mvx, mvy) col

instance Arbitrary Color where
  arbitrary = do
    r <- choose (0, 1)
    g <- choose (0, 1)
    b <- choose (0, 1)
    a <- choose (0, 1)
    return $ makeColor r g b a

prop_initState_tupleConsistent
  :: (Int, (Float,Float), (Float,Float), (Float,Float), (Float,Float), (Float,Float), Color)
  -> Bool
prop_initState_tupleConsistent tuple =
  let st = initState tuple
      (i, p, v, d, vI, mV, c) = tuple
  in ballID st == i && pos st == p && vel st == v

main :: IO ()
main = quickCheck prop_initState_tupleConsistent
