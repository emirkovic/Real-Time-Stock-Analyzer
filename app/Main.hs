module Main where

import StockData
import Control.Monad (forever)
import Control.Concurrent (threadDelay)
import Data.Time.Clock (getCurrentTime, diffUTCTime, UTCTime)
import System.Exit (exitFailure)
import Data.List (intercalate)
import Data.Maybe (catMaybes)
import Control.Applicative (liftA2)

main :: IO ()
main = do
  putStrLn "Starting real-time stock analyzer..."
  putStrLn "Press Ctrl+C to exit\n"
  forever $ do
    cycleStart <- getCurrentTime
    putStrLn "=== New analysis cycle ==="

    results <- mapM analyze ["AAPL", "GOOGL", "MSFT"]

    let validData = catMaybes results

    if null validData
      then putStrLn "Error: No valid data received in this cycle" >> exitFailure
      else do
        putStrLn ("Successfully processed: " ++ intercalate ", " (map (\(s,_,_) -> s) validData))
        plotCombined validData

    cycleEnd <- getCurrentTime
    let totalTime = realToFrac (diffUTCTime cycleEnd cycleStart) :: Double
        remainingTime = max 0 (300 - totalTime)
    putStrLn "\nCycle stats:"
    putStrLn ("- Processed " ++ show (length validData) ++ "/3 stocks")
    putStrLn ("- Total processing time: " ++ show totalTime ++ "s")
    putStrLn ("- Next update in " ++ show remainingTime ++ " seconds\n")
    threadDelay (floor (remainingTime * 1000000))

analyze :: String -> IO (Maybe (String, [(UTCTime, Double)], [(UTCTime, Double)]) )
analyze symbol = do
  putStrLn $ "\nStarting processing for " ++ symbol
  h <- downloadHistoricalData symbol
  s <- downloadSMAData symbol
  let combined = liftA2 (,) h s
  case combined of
    Right (h', s') -> do
      putStrLn $ "Successfully processed " ++ symbol
      return $ processStock symbol (Right (h', s'))
    Left err -> do
      putStrLn $ "Failed to process " ++ symbol ++ ": " ++ err
      return Nothing
