{-# LANGUAGE ScopedTypeVariables #-}

module TestInput where

import Test.QuickCheck

-- Example: A pure validation for ball size.
validBallSize :: Int -> Int -> Bool
validBallSize n maxR = n >= 1 && n <= maxR

prop_validBallSize_inRange :: Int -> Positive Int -> Property
prop_validBallSize_inRange n (Positive maxR) =
  (n >= 1 && n <= maxR) ==> validBallSize n maxR

main :: IO ()
main = quickCheck prop_validBallSize_inRange
