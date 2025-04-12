module Main where

import StockData
import Control.Monad (forever)
import Control.Concurrent (threadDelay)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import System.Exit (exitFailure)
import Data.List (intercalate)
import Data.Maybe (catMaybes)

main :: IO ()
main = 
  putStrLn "Starting real-time stock analyzer..."
    >> putStrLn "Press Ctrl+C to exit\n"
    >> forever (
         getCurrentTime
           >>= \cycleStart ->
             putStrLn "=== New analysis cycle ==="
               >> mapM processStock ["AAPL", "GOOGL", "MSFT"]
               >>= \results ->
                 let validData = catMaybes results
                 in if null validData
                      then putStrLn "Error: No valid data received in this cycle" >> exitFailure
                      else putStrLn ("Successfully processed: " ++ intercalate ", " (map (\(s,_,_) -> s) validData))
                           >> plotCombined validData
                           >> getCurrentTime
                           >>= \cycleEnd ->
                             let totalTime = realToFrac (diffUTCTime cycleEnd cycleStart) :: Double
                                 remainingTime = max 0 (300 - totalTime)
                             in putStrLn "\nCycle stats:"
                                >> putStrLn ("- Processed " ++ show (length validData) ++ "/3 stocks")
                                >> putStrLn ("- Total processing time: " ++ show totalTime ++ "s")
                                >> putStrLn ("- Next update in " ++ show remainingTime ++ " seconds\n")
                                >> threadDelay (floor (remainingTime * 1000000))
       )