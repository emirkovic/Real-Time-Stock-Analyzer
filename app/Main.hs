module Main where

import StockData
import Control.Monad (forever)
import Control.Concurrent (threadDelay)
import Data.Time.Clock (getCurrentTime, diffUTCTime, UTCTime)
import System.Exit (exitFailure)
import Data.List (intercalate)
import Graphics.Rendering.Chart.Easy as CE
import Graphics.Rendering.Chart.Backend.Diagrams (renderableToFile)

-- Combined data structure
processStock :: String -> IO (Either String (String, [(UTCTime, Double)], [(UTCTime, Double)]))
processStock symbol = do
  historicalResult <- fetchHistoricalData symbol
  smaResult <- fetchSMAData symbol
  case (historicalResult, smaResult) of
    (Right h, Right s) -> return $ Right (symbol, h, s)
    (Left err, _) -> return $ Left ("[" ++ symbol ++ "] Historical: " ++ err)
    (_, Left err) -> return $ Left ("[" ++ symbol ++ "] SMA: " ++ err)

plotCombined :: [(String, [(UTCTime, Double)], [(UTCTime, Double)])] -> IO ()
plotCombined stocksData = do
  let layout = CE.execEC $ do
        CE.layout_title .= "Real-Time Stock Analysis"
        CE.layout_x_axis . CE.laxis_title .= "Date"
        CE.layout_y_axis . CE.laxis_title .= "Price (USD)"
        mapM_ (\(symbol, prices, sma) -> do
          CE.plot (CE.line (symbol ++ " Price") [prices])
          CE.plot (CE.line (symbol ++ " SMA-10") [sma])) stocksData
  _ <- renderableToFile def "combined_chart.svg" (CE.toRenderable layout)
  putStrLn "Chart saved: combined_chart.svg"

main :: IO ()
main = do
  putStrLn "Starting real-time stock analyzer..."
  putStrLn "Press Ctrl+C to exit\n"
  forever $ do
    cycleStart <- getCurrentTime
    putStrLn "=== New analysis cycle ==="

    results <- mapM processStock ["AAPL", "GOOGL", "MSFT"]

    let (errors, valids) = foldr
          (\res (es, vs) -> case res of
              Left err -> (err:es, vs)
              Right val -> (es, val:vs))
          ([], [])
          results

    mapM_ putStrLn errors

    if null valids
      then putStrLn "Error: No valid data received." >> exitFailure
      else do
        putStrLn ("Successfully processed: " ++ intercalate ", " (map (\(s,_,_) -> s) valids))
        plotCombined valids

    cycleEnd <- getCurrentTime
    let totalTime = realToFrac (diffUTCTime cycleEnd cycleStart) :: Double
        remainingTime = max 0 (300 - totalTime)
    putStrLn "\nCycle stats:"
    putStrLn ("- Processed " ++ show (length valids) ++ "/3 stocks")
    putStrLn ("- Total processing time: " ++ show totalTime ++ "s")
    putStrLn ("- Next update in " ++ show remainingTime ++ " seconds\n")
    threadDelay (floor (remainingTime * 1000000))
