module Main where

import StockData
import Control.Monad (forever)
import Control.Concurrent (threadDelay)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import System.Exit (exitFailure)
import Data.List (intercalate)
import Data.Maybe (catMaybes)

main :: IO ()
main = do
  let stocks = ["AAPL", "GOOGL", "MSFT"]
  putStrLn "Starting real-time stock analyzer..."
  putStrLn "Press Ctrl+C to exit\n"
  
  forever $ do
    cycleStart <- getCurrentTime
    putStrLn "=== New analysis cycle ==="
    
    results <- mapM processStock stocks
    let validData = catMaybes results
    
    if null validData
      then do
        putStrLn "Error: No valid data received in this cycle"
        exitFailure
      else do
        putStrLn $ "Successfully processed: " ++ intercalate ", " (map (\(s,_,_) -> s) validData)
        plotCombined validData
        
    cycleEnd <- getCurrentTime
    let totalTime = realToFrac (diffUTCTime cycleEnd cycleStart) :: Double
        remainingTime = max 0 (300 - totalTime)  -- 5 minute
    
    putStrLn $ "\nCycle stats:"
    putStrLn $ "- Processed " ++ show (length validData) ++ "/" ++ show (length stocks) ++ " stocks"
    putStrLn $ "- Total processing time: " ++ show totalTime ++ "s"
    putStrLn $ "- Next update in " ++ show remainingTime ++ " seconds\n"
    
    threadDelay (floor (remainingTime * 1000000))