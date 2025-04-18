{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}

module StockData
  ( fetchHistoricalData
  , fetchSMAData
  , parseDate
  , TimeSeries(..)
  , TechnicalAnalysis(..)
  , SMADatum(..)
  ) where

import Control.Exception (SomeException, handle)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Time (UTCTime)
import Data.Time.Format (parseTimeOrError, defaultTimeLocale)
import Network.HTTP.Client qualified as Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Simple (parseRequest)
import Network.HTTP.Types.Status (status429)
import Data.Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.List (isInfixOf)

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

-- Date Parsing
parseDate :: String -> UTCTime
parseDate = parseTimeOrError True defaultTimeLocale "%Y-%m-%d"

-- Pure fetcher wrapper
fetchJson :: FromJSON a => String -> IO (Either String a)
fetchJson url = handle handler $ do
  manager <- Client.newManager tlsManagerSettings
  request <- parseRequest url
  response <- Client.httpLbs request manager
  let body = Client.responseBody response
      status = Client.responseStatus response
  if status == status429
    then return $ Left "API rate limit exceeded"
    else case decode body of
      Just (Object o)
        | KM.member "Error Message" o -> return $ Left "API error: Invalid request"
        | KM.member "Information" o -> return $ Left "API error: Premium endpoint"
      _ -> case eitherDecode body of
        Left err -> return $ Left err
        Right val -> return $ Right val
  where
    handler :: SomeException -> IO (Either String a)
    handler e = return $ Left ("HTTP error: " ++ show e)

-- Fetchers return pure Either
fetchHistoricalData :: String -> IO (Either String [(UTCTime, Double)])
fetchHistoricalData symbol = do
  let url = "https://www.alphavantage.co/query?function=TIME_SERIES_DAILY&symbol="
         ++ symbol ++ "&apikey=ADTW707HBTBIH3EI&outputsize=compact"
  fetchJson url >>= \case
    Right (TimeSeries _ ts) ->
      return $ Right $ map (\(date, DailyData{close}) -> (parseDate date, close)) (Map.toList ts)
    Left err -> return $ Left err

fetchSMAData :: String -> IO (Either String [(UTCTime, Double)])
fetchSMAData symbol = do
  let url = "https://www.alphavantage.co/query?function=SMA&symbol="
         ++ symbol ++ "&interval=daily&time_period=10&series_type=close&apikey=ADTW707HBTBIH3EI"
  fetchJson url >>= \case
    Right (TechnicalAnalysis _ ta) ->
      return $ Right $ map (\(date, SMADatum{smaValue}) -> (parseDate date, smaValue)) (Map.toList ta)
    Left err -> return $ Left err