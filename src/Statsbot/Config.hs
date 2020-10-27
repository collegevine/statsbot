module Statsbot.Config (
    ConfigErrorMessage(..),
    ConfigFile(..),
    loadRowConfigurations
) where

import Protolude

import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Data.Yaml (decodeFileEither)

import Statsbot.Types (
    MetricSource,
    Row(..),
    Title(..),
    Link(..),
    PendingMetrics(..)
    )
import Statsbot.Source

{-| A config file example
 -
 -
 - rows:
 -   - type: psql
 -     title: "A great metric"
 -     link: "https://google.com"
 -     details:
 -       connection: PSQL_DB_CONN
 -       query: |
 -         select count(*) as num, date
 -         from my_table
 -         where date > now - interval '1 month'
 -         group by date;
 -
 -  - type: psql
 -     title: "Another metric"
 -     link: "https://apple.com"
 -     details:
 -       connection: PSQL_DB_CONN
 -       query: |
 -         select count(*) as num, date
 -         from my_table
 -         where date > now - interval '1 month'
 -         group by date;
 -}


newtype ConfigErrorMessage =
    ConfigLoadError {errorMessage :: Text }
    deriving Show

loadRowConfigurations ::
    FilePath
    -> IO (Either ConfigErrorMessage (ConfigFile Row))
loadRowConfigurations configFile = do
    res <- decodeFileEither configFile
    case res of
        Left err -> pure $ Left (ConfigLoadError "whoops")
        Right conf -> pure . Right $ translateConfigRows conf
    where
        toRow :: RowSpec -> Row
        toRow (PSQLRow t l src) = Row t l $ PendingMetrics src

        translateConfigRows conf = ConfigFile {
              rows = toRow <$> rows conf
            , reportTitle = reportTitle conf
            , historyWindowDays = historyWindowDays conf
            , targetURL = targetURL conf
            }

data ConfigFile a = ConfigFile {
      rows :: [a]
    , reportTitle :: Text
    , historyWindowDays :: Int
    , targetURL :: Text
    }

-- TODO switch over to generics
instance FromJSON (ConfigFile RowSpec) where
    parseJSON = withObject "Config file" $ \raw -> do
        rowSpecs <- raw .: "rows"
        reportTitle <- raw .: "report_title"
        historyWindowDays <- raw .: "history_window_days"
        targetURL <- raw .: "target_url"
        pure $ ConfigFile rowSpecs reportTitle historyWindowDays targetURL

data RowSpec =
    PSQLRow Title Link PostgresMetricSource

instance FromJSON RowSpec where
    parseJSON = withObject "RowSpec" $ \row -> do
        (tpe :: Text) <- row .: "type"
        title <- row .: "title"
        link <- row .: "link"
        case tpe of
            "psql" -> PSQLRow title link <$> row .: "details"
            _ -> typeMismatch "RowSpec" (Object row)
