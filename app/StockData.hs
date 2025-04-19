{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Functor law" #-}

module StockData
  ( processStock,
    plotCombined,
  )
where

import Control.Concurrent (threadDelay)
import Control.Exception (handle)
import Control.Lens ((.=))
import Control.Monad (forM_)
import Data.Aeson hiding ((.=))
import Data.Aeson.KeyMap qualified as KM
import Data.Default.Class (def)
import Data.List (isInfixOf)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import Data.Time.Format (defaultTimeLocale, parseTimeOrError)
import Graphics.Rendering.Chart.Backend.Diagrams (renderableToFile)
import Graphics.Rendering.Chart.Easy qualified as CE
import Network.HTTP.Client qualified as Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Simple (parseRequest)
import Network.HTTP.Types (statusCode)

-- API Configuration
apiKey :: String
apiKey = "ADTW707HBTBIH3EI"

-- Data Types
data TimeSeries = TimeSeries
  { tsMetaData :: HistoricalMetaData,
    timeSeriesDaily :: Map String DailyData
  }
  deriving (Show)

data HistoricalMetaData = HistoricalMetaData
  { information :: String,
    symbol :: String,
    lastRefreshed :: String,
    outputSize :: String,
    timeZone :: String
  }
  deriving (Show)

data DailyData = DailyData
  { open :: Double,
    high :: Double,
    low :: Double,
    close :: Double,
    volume :: Int
  }
  deriving (Show)

data TechnicalAnalysis = TechnicalAnalysis
  { taMetaData :: SMAMetaData,
    technicalAnalysis :: Map String SMADatum
  }
  deriving (Show)

data SMAMetaData = SMAMetaData
  { smaSymbol :: String,
    indicator :: String,
    lastRefreshedSMA :: String,
    interval :: String,
    timePeriod :: Int,
    seriesType :: String,
    timeZoneSMA :: String
  }
  deriving (Show)

data SMADatum = SMADatum
  { smaValue :: Double
  }
  deriving (Show)

-- JSON Parsing Instances
instance FromJSON TimeSeries where
  parseJSON = withObject "TimeSeries" $ \v ->
    TimeSeries
      <$> v .: "Meta Data"
      <*> v .: "Time Series (Daily)"

instance FromJSON HistoricalMetaData where
  parseJSON = withObject "HistoricalMetaData" $ \v ->
    HistoricalMetaData
      <$> v .: "1. Information"
      <*> v .: "2. Symbol"
      <*> v .: "3. Last Refreshed"
      <*> v .: "4. Output Size"
      <*> v .: "5. Time Zone"

instance FromJSON DailyData where
  parseJSON = withObject "DailyData" $ \v ->
    DailyData
      <$> (read <$> v .: "1. open")
      <*> (read <$> v .: "2. high")
      <*> (read <$> v .: "3. low")
      <*> (read <$> v .: "4. close")
      <*> (read <$> v .: "5. volume")

instance FromJSON TechnicalAnalysis where
  parseJSON = withObject "TechnicalAnalysis" $ \v ->
    TechnicalAnalysis
      <$> v .: "Meta Data"
      <*> v .: "Technical Analysis: SMA"

instance FromJSON SMAMetaData where
  parseJSON = withObject "SMAMetaData" $ \v ->
    SMAMetaData
      <$> v .: "1: Symbol"
      <*> v .: "2: Indicator"
      <*> v .: "3: Last Refreshed"
      <*> v .: "4: Interval"
      <*> v .: "5: Time Period"
      <*> v .: "6: Series Type"
      <*> v .: "7: Time Zone"

instance FromJSON SMADatum where
  parseJSON = withObject "SMADatum" $ \v ->
    SMADatum
      <$> (read <$> v .: "SMA")

-- Parse date (cista funkcija)
parseDate :: String -> UTCTime
parseDate = parseTimeOrError True defaultTimeLocale "%Y-%m-%d"

-- Cista obrada podataka
processHistorical :: TimeSeries -> [(UTCTime, Double)]
processHistorical (TimeSeries _ ts) =
  map (\(date, DailyData {close}) -> (parseDate date, close)) (Map.toList ts)

processSMA :: TechnicalAnalysis -> [(UTCTime, Double)]
processSMA (TechnicalAnalysis _ ts) =
  map (\(date, SMADatum {smaValue}) -> (parseDate date, smaValue)) (Map.toList ts)

-- IO funkcija samo za dohvat i dekodiranje
fetchJson :: forall a. (FromJSON a) => String -> IO (Either String a)
fetchJson originalUrl = retry 3
  where
    retry :: Int -> IO (Either String a)
    retry 0 = return $ Left "Max retries reached"
    retry n = do
      (result, _) <- fetchOnce
      case result of
        Left err | shouldRetry err -> do
          let delay = 2 ^ (4 - n) * 1000000
          threadDelay delay
          retry (n - 1)
        _ -> return result

    fetchOnce :: IO (Either String a, Double)
    fetchOnce = handle handler $ do
      manager <- Client.newManager tlsManagerSettings {Client.managerResponseTimeout = Client.responseTimeoutMicro 5000000}
      request <- parseRequest originalUrl
      startTime <- getCurrentTime
      response <- Client.httpLbs request manager
      endTime <- getCurrentTime
      let latency = realToFrac (diffUTCTime endTime startTime) :: Double
          status = Client.responseStatus response
          body = Client.responseBody response
      if statusCode status == 429
        then return (Left "API rate limit exceeded", latency)
        else case decode body of
          Just (Object o) | KM.member "Error Message" o -> return (Left "API error: Invalid request", latency)
          Just (Object o) | KM.member "Information" o -> return (Left "API error: Premium endpoint", latency)
          _ -> case eitherDecode body of
            Left err -> return (Left err, latency)
            Right (val :: a) -> return (Right val, latency)

    handler :: Client.HttpException -> IO (Either String a, Double)
    handler e = return (Left ("HTTP error: " ++ show e), 0)

    shouldRetry :: String -> Bool
    shouldRetry err =
      any
        (`isInfixOf` err)
        ["API rate limit exceeded", "HttpException", "timed out"]

-- Cisti dio + kombinacija s IO
fetchHistoricalData :: String -> IO (Either String [(UTCTime, Double)])
fetchHistoricalData symbol = do
  let url = "https://www.alphavantage.co/query?function=TIME_SERIES_DAILY&symbol=" ++ symbol ++ "&apikey=" ++ apiKey ++ "&outputsize=compact"
  result <- fetchJson url
  return $ fmap processHistorical result

fetchSMAData :: String -> IO (Either String [(UTCTime, Double)])
fetchSMAData symbol = do
  let url = "https://www.alphavantage.co/query?function=SMA&symbol=" ++ symbol ++ "&interval=daily&time_period=10&series_type=close&apikey=" ++ apiKey
  result <- fetchJson url
  return $ fmap processSMA result

-- Kombinacija (IO samo za dohvat, obrada cista)
processStock :: String -> IO (Maybe (String, [(UTCTime, Double)], [(UTCTime, Double)]))
processStock symbol = do
  putStrLn $ "\nStarting processing for " ++ symbol
  histResult <- fetchHistoricalData symbol
  smaResult <- fetchSMAData symbol
  case (histResult, smaResult) of
    (Right h, Right s) -> do
      putStrLn $ "Successfully processed " ++ symbol
      return $ Just (symbol, h, s)
    _ -> do
      putStrLn $ "Failed to process " ++ symbol
      return Nothing

-- Visualization (IO)
plotCombined :: [(String, [(UTCTime, Double)], [(UTCTime, Double)])] -> IO ()
plotCombined stocksData = do
  let layout = CE.execEC $ do
        CE.layout_title .= "Real-Time Stock Analysis"
        CE.layout_x_axis . CE.laxis_title .= "Date"
        CE.layout_y_axis . CE.laxis_title .= "Price (USD)"
        forM_ stocksData $ \(symbol, prices, sma) -> do
          CE.plot (CE.line (symbol ++ " Price") [prices])
          CE.plot (CE.line (symbol ++ " SMA-10") [sma])

  _ <- renderableToFile def "combined_chart.svg" (CE.toRenderable layout)
  putStrLn "Updated combined chart saved as combined_chart.svg"