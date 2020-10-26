module Statsbot.Config where

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
import Statsbot.Source.PostgresSQL

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


newtype ErrorMessage =
    ConfigLoadError {errorMessage :: Text }
    deriving Show

loadRowConfigurations ::
    FilePath
    -> IO (Either ErrorMessage [Row])
loadRowConfigurations configFile = do
    res <- decodeFileEither configFile
    case res of
        Left err -> pure $ Left (ConfigLoadError "whoops")
        Right (ConfigFile rows) -> pure . Right $ toRow <$> rows
    where
        toRow :: RowSpec -> Row
        toRow (PSQLRow t l src) = Row t l $ PendingMetrics src

newtype ConfigFile = ConfigFile { rows :: [RowSpec] }

instance FromJSON ConfigFile where
    parseJSON = withObject "Config file" $ \raw -> do
        rowSpecs <- raw .: "rows"
        pure $ ConfigFile rowSpecs


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
