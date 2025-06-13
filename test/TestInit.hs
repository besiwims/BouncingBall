{-# LANGUAGE ScopedTypeVariables #-}

module TestInit where

import Test.QuickCheck
import Control.Monad.Reader
import Animation.Init
import Animation.Environment
import Animation.State

instance Arbitrary Environment where
  arbitrary = do
    w <- choose (300, 3000)
    h <- choose (200, 2000)
    r <- choose (1, 50)
    n <- choose (1, 30)
    return $ getDefaultEnv { frameDimension = (w, h), ballRadius = r, ballCounts = n }

prop_initBalls_count :: Environment -> Property
prop_initBalls_count env =
  ioProperty $ do
    balls <- runReaderT initBalls env
    return $ length balls == ballCounts env

main :: IO ()
main = quickCheck prop_initBalls_count
