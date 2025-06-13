module Main where

import qualified TestEnvironment
import qualified TestInput
import qualified TestDraw
import qualified TestInit
import qualified TestState
import qualified TestGame
import System.IO (hFlush, stdout)
import Data.Time.Clock

runTest :: String -> IO () -> IO ()
runTest name test = do
  putStrLn $ "\n==== " ++ name ++ " ===="
  hFlush stdout
  start <- getCurrentTime
  test
  end <- getCurrentTime
  putStrLn $ "-- " ++ name ++ " completed in " ++ show (diffUTCTime end start)

main :: IO ()
main = do
  runTest "Environment Tests" TestEnvironment.main
  runTest "Input Tests"       TestInput.main
  runTest "Draw Tests"        TestDraw.main
  runTest "Init Tests"        TestInit.main
  runTest "State Tests"       TestState.main
  runTest "Game Tests"        TestGame.main
  putStrLn "\nAll tests finished!"
