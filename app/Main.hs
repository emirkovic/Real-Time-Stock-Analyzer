module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Data.List (intercalate)
import Data.Maybe (catMaybes)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import StockData
import System.Exit (exitFailure)

-- Pomocna cista funkcija za statistiku ciklusa
formatStats :: Double -> Int -> Int -> String
formatStats totalTime successCount totalCount =
  "\nCycle stats:\n"
    ++ "- Processed "
    ++ show successCount
    ++ "/"
    ++ show totalCount
    ++ " stocks\n"
    ++ "- Total processing time: "
    ++ show totalTime
    ++ "s\n"
    ++ "- Next update in "
    ++ show (max 0 (300 - totalTime))
    ++ " seconds\n"

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
        putStrLn $ "Successfully processed: " ++ intercalate ", " (map (\(s, _, _) -> s) validData)
        plotCombined validData

    cycleEnd <- getCurrentTime
    let totalTime = realToFrac (diffUTCTime cycleEnd cycleStart) :: Double
    putStrLn $ formatStats totalTime (length validData) (length stocks)
    threadDelay (floor (max 0 (300 - totalTime) * 1000000))