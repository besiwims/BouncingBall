{-# LANGUAGE ScopedTypeVariables #-}

module TestEnvironment where

import Test.QuickCheck
import Animation.Environment

instance Arbitrary Environment where
  arbitrary = do
    w <- choose (300, 3000)
    h <- choose (200, 2000)
    r <- choose (1, 50)
    return $ getDefaultEnv { frameDimension = (w, h), ballRadius = r }

prop_wBoundCorrect :: Environment -> Bool
prop_wBoundCorrect env =
  wBound env == fromIntegral (fst (frameDimension env)) / 2

prop_xBoundWithin :: Environment -> Bool
prop_xBoundWithin env =
  xBound env <= wBound env

main :: IO ()
main = do
  quickCheck prop_wBoundCorrect
  quickCheck prop_xBoundWithin
