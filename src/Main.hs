module Main where

import System.IO                  (hSetBuffering, stdout, BufferMode(NoBuffering))
import System.Environment         (getArgs)

import Graphics.Gloss             (Display(InWindow), black)
import Graphics.Gloss.Interface.IO.Game
  ( playIO )

import Animation.Environment      ( frameDimension, framePerSecond )
import Animation.Init             ( initGame )
import Animation.Game             ( drawGame, handleInput, stepGame )

main :: IO ()
main = do
  -- Don’t buffer stdout so prompts show immediately
  hSetBuffering stdout NoBuffering

  -- Initialize env & state
  (env, initialState) <- initGame =<< getArgs

  -- Unpack window size & FPS
  let (w, h)  = frameDimension env
      display = InWindow "BouncingBall" (w, h) (100, 100)
      fps     = framePerSecond env

  -- Run the Gloss play loop
  playIO
    display
    black
    fps
    initialState
    (drawGame env)
    (handleInput env)
    (stepGame env)
