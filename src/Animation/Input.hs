{-# LANGUAGE BlockArguments #-}

module Animation.Input
  ( getFrameW
  , getFrameH
  , getBallSize
  , getBallInitVel
  , getMaxVelocity
  , getWallCharges
  , getFileLogging
  , getBallCount
  ) where

import Control.Monad          (when)
import Control.Monad.Reader   (ReaderT, ask, runReaderT)
import Control.Monad.IO.Class (liftIO)
import Data.Char              (isDigit, toUpper)

-- | Initial window width
getFrameW :: IO Int
getFrameW = runReaderT getWH ("Width [min.250, default 1500]: ", 1500)

-- | Initial window height
getFrameH :: IO Int
getFrameH = runReaderT getWH ("Height [min.250, default 750]: ", 750)

-- | Ball radius (max limited by argument)
getBallSize :: Int -> IO Float
getBallSize = runReaderT getSize

-- | Initial ball speed
getBallInitVel :: IO Float
getBallInitVel = runReaderT getSpeed ("Initial speed [default 5]: ", 5)

-- | Maximum ball speed
getMaxVelocity :: IO Float
getMaxVelocity = runReaderT getSpeed ("Max. speed [default 500]: ", 500)

-- | Wall “charge” (velocity increment on bounce)
getWallCharges :: IO Float
getWallCharges = runReaderT getSpeed ("Wall charge [default 50]: ", 50)

-- | Toggle file logging
getFileLogging :: IO Bool
getFileLogging = do
  putStr "Enable logging? [Y/N, default N]: "
  inp <- getLine
  case fmap toUpper inp of
    ""  -> return False
    "Y" -> return True
    "N" -> return False
    _   -> putStrLn "Please enter Y or N." >> getFileLogging

-- | Number of balls, with max + first-run message
getBallCount :: Int -> Bool -> IO Int
getBallCount = curry $ runReaderT getCount

-- ----------------------------------------------------------------------------
-- Private reader actions
-- ----------------------------------------------------------------------------

-- | Generic width/height prompt
getWH :: ReaderT (String, Int) IO Int
getWH = do
  (lbl, def) <- ask
  liftIO $ putStr lbl
  inp <- liftIO getLine
  if null inp then do
    liftIO $ print def
    return def
  else if all isDigit inp then
    let n = read inp in
      if n < 250
        then liftIO (putStr "Min.250. ") >> getWH
        else return n
  else
    liftIO (putStr "Must be a non-negative integer. ") >> getWH

-- | Ball‐size prompt
getSize :: ReaderT Int IO Float
getSize = do
  maxR <- ask
  liftIO $ putStr $ "Ball radius [1.." ++ show maxR ++ ", default 5]: "
  inp <- liftIO getLine
  if null inp then do
    liftIO $ print (5 :: Int)
    return 5.0
  else if all isDigit inp then
    let n = read inp in
      if n < 1 || n > maxR
        then liftIO (putStrLn $ "Must be between 1 and " ++ show maxR) >> getSize
        else return (fromIntegral n)
  else
    liftIO (putStr "Must be a non-negative integer. ") >> getSize

-- | Generic float-valued prompt (runs on Int input)
getSpeed :: ReaderT (String, Float) IO Float
getSpeed = do
  (lbl, def) <- ask
  liftIO $ putStr lbl
  inp <- liftIO getLine
  if null inp then do
    liftIO $ print (truncate def)
    return def
  else if all isDigit inp then
    return $ fromIntegral (read inp)
  else
    liftIO (putStr "Must be a non-negative integer. ") >> getSpeed

-- | Ball-count prompt with optional “first run” message
getCount :: ReaderT (Int, Bool) IO Int
getCount = do
  (maxC, firstRun) <- ask
  let defC = min maxC 500
      maxS = show maxC
  when firstRun $
    liftIO $ putStrLn "Surprise! Creating your log file..."
  liftIO $ putStr $ "How many balls [1.." ++ maxS ++ ", default " ++ show defC ++ "]: "
  inp <- liftIO getLine
  if null inp then do
    liftIO $ print defC
    return defC
  else if all isDigit inp then
    let n = read inp in
      if n < 1 || n > maxC
        then liftIO (putStrLn $ "Must be between 1 and " ++ maxS) >> getCount
        else return n
  else
    liftIO (putStr "Must be a non-negative integer. ") >> getCount
