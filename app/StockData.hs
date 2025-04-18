{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}

module StockData
  ( downloadHistoricalData
  , downloadSMAData
  , processStock
  , plotCombined
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (handle)
import Control.Monad (foldM_)
import Control.Lens ((.=))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Time (UTCTime, getCurrentTime, diffUTCTime)
import Data.Time.Format (parseTimeOrError, defaultTimeLocale)
import qualified Graphics.Rendering.Chart.Easy as CE
import Graphics.Rendering.Chart.Backend.Diagrams (renderableToFile)
import Data.Default.Class (def)
import Network.HTTP.Types (statusCode)
import Network.HTTP.Client qualified as Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Simple (parseRequest)
import Data.Aeson hiding ((.=))
import Data.Aeson.KeyMap qualified as KM
import Data.List (isInfixOf)


-- API Configuration
apiKey :: String
apiKey = "ADTW707HBTBIH3EI"

-- Enhanced HTTP Client with Retries and Latency Tracking
stockHttpJSON :: forall a. FromJSON a => String -> IO (Either String a)
stockHttpJSON originalUrl = retry 3
  where
    retry :: Int -> IO (Either String a)
    retry 0 = return $ Left "Max retries reached"
    retry n = stockHttpJSONOnce >>= handleRetry n

    handleRetry n (Left err, _) 
      | shouldRetry err = 
          threadDelay delay >> retry (n-1)
      where delay = 2 ^ (4 - n) * 1000000
    handleRetry _ (result, _) = return result

    stockHttpJSONOnce :: IO (Either String a, Double)
    stockHttpJSONOnce = handle handler $ do
      manager <- Client.newManager tlsManagerSettings 
        { Client.managerResponseTimeout = Client.responseTimeoutMicro 5000000 }
      request <- parseRequest originalUrl
      startTime <- getCurrentTime
      response <- Client.httpLbs request manager
      endTime <- getCurrentTime
      let latency = realToFrac (diffUTCTime endTime startTime)
          status = Client.responseStatus response
          body = Client.responseBody response
      if statusCode status == 429
        then return (Left "API rate limit exceeded", latency)
        else case decode body of
          Just (Object o) 
            | KM.member "Error Message" o -> return (Left "API error: Invalid request", latency)
            | KM.member "Information" o -> return (Left "API error: Premium endpoint", latency)
          _ -> case eitherDecode body of
            Left err -> return (Left err, latency)
            Right val -> return (Right val, latency)

    shouldRetry err = any (`isInfixOf` err) ["API rate limit exceeded", "HttpException", "timed out"]
    handler :: Client.HttpException -> IO (Either String a, Double)
    handler e = return (Left ("HTTP error: " ++ show e), 0)

-- Data Types
data TimeSeries = TimeSeries
  { tsMetaData :: HistoricalMetaData
  , timeSeriesDaily :: Map String DailyData
  } deriving (Show)

data HistoricalMetaData = HistoricalMetaData
  { information :: String
  , symbol :: String
  , lastRefreshed :: String
  , outputSize :: String
  , timeZone :: String
  } deriving (Show)

data DailyData = DailyData
  { open   :: Double
  , high   :: Double
  , low    :: Double
  , close  :: Double
  , volume :: Int
  } deriving (Show)

data TechnicalAnalysis = TechnicalAnalysis
  { taMetaData :: SMAMetaData
  , technicalAnalysis :: Map String SMADatum
  } deriving (Show)

data SMAMetaData = SMAMetaData
  { smaSymbol :: String
  , indicator :: String
  , lastRefreshedSMA :: String
  , interval :: String
  , timePeriod :: Int
  , seriesType :: String
  , timeZoneSMA :: String
  } deriving (Show)

data SMADatum = SMADatum
  { smaValue :: Double
  } deriving (Show)

-- JSON Parsing Instances
instance FromJSON TimeSeries where
  parseJSON = withObject "TimeSeries" $ \v -> TimeSeries
    <$> v .: "Meta Data"
    <*> v .: "Time Series (Daily)"

instance FromJSON HistoricalMetaData where
  parseJSON = withObject "HistoricalMetaData" $ \v -> HistoricalMetaData
    <$> v .: "1. Information"
    <*> v .: "2. Symbol"
    <*> v .: "3. Last Refreshed"
    <*> v .: "4. Output Size"
    <*> v .: "5. Time Zone"

instance FromJSON DailyData where
  parseJSON = withObject "DailyData" $ \v -> DailyData
    <$> (read <$> v .: "1. open")
    <*> (read <$> v .: "2. high")
    <*> (read <$> v .: "3. low")
    <*> (read <$> v .: "4. close")
    <*> (read <$> v .: "5. volume")

instance FromJSON TechnicalAnalysis where
  parseJSON = withObject "TechnicalAnalysis" $ \v -> TechnicalAnalysis
    <$> v .: "Meta Data"
    <*> v .: "Technical Analysis: SMA"

instance FromJSON SMAMetaData where
  parseJSON = withObject "SMAMetaData" $ \v -> SMAMetaData
    <$> v .: "1: Symbol"
    <*> v .: "2: Indicator"
    <*> v .: "3: Last Refreshed"
    <*> v .: "4: Interval"
    <*> v .: "5: Time Period"
    <*> v .: "6: Series Type"
    <*> v .: "7: Time Zone"

instance FromJSON SMADatum where
  parseJSON = withObject "SMADatum" $ \v -> SMADatum
    <$> (read <$> v .: "SMA")

-- Pure transformation
parseDate :: String -> UTCTime
parseDate = parseTimeOrError True defaultTimeLocale "%Y-%m-%d"

extractHistoricalPrices :: TimeSeries -> [(UTCTime, Double)]
extractHistoricalPrices (TimeSeries _ ts) =
  map (\(date, DailyData{close}) -> (parseDate date, close)) (Map.toList ts)

extractSMAValues :: TechnicalAnalysis -> [(UTCTime, Double)]
extractSMAValues (TechnicalAnalysis _ ta) =
  map (\(date, SMADatum{smaValue}) -> (parseDate date, smaValue)) (Map.toList ta)

-- IO Boundary
downloadHistoricalData :: String -> IO (Either String [(UTCTime, Double)])
downloadHistoricalData symbol =
  stockHttpJSON url >>= \case
    Right ts -> return $ Right (extractHistoricalPrices ts)
    Left err -> return $ Left err
  where
    url = "https://www.alphavantage.co/query?function=TIME_SERIES_DAILY&symbol="
        ++ symbol ++ "&apikey=" ++ apiKey ++ "&outputsize=compact"

downloadSMAData :: String -> IO (Either String [(UTCTime, Double)])
downloadSMAData symbol =
  stockHttpJSON url >>= \case
    Right ta -> return $ Right (extractSMAValues ta)
    Left err -> return $ Left err
  where
    url = "https://www.alphavantage.co/query?function=SMA&symbol="
        ++ symbol ++ "&interval=daily&time_period=10&series_type=close&apikey=" ++ apiKey

-- Pure data combiner
processStock :: String -> Either String ([(UTCTime, Double)], [(UTCTime, Double)]) -> Maybe (String, [(UTCTime, Double)], [(UTCTime, Double)])
processStock symbol (Right (h, s)) = Just (symbol, h, s)
processStock _ (Left _) = Nothing

-- Visualization
plotCombined :: [(String, [(UTCTime, Double)], [(UTCTime, Double)])] -> IO ()
plotCombined stocksData =
  let layout = CE.execEC $ do
        CE.layout_title .= "Real-Time Stock Analysis"
        CE.layout_x_axis . CE.laxis_title .= "Date"
        CE.layout_y_axis . CE.laxis_title .= "Price (USD)"
        foldM_ (\_ (symbol, prices, sma) -> do
          CE.plot (CE.line (symbol ++ " Price") [prices]) 
          CE.plot (CE.line (symbol ++ " SMA-10") [sma])) () stocksData
  in renderableToFile def "combined_chart.svg" (CE.toRenderable layout)
     >> putStrLn "Updated combined chart saved as combined_chart.svg"
