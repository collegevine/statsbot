module Statsbot.Source.PostgresSQL where

import Protolude
import Prelude (String)

import Data.String (IsString(..))
import Data.Aeson
import qualified Data.Text as T
import Database.PostgreSQL.Simple
import System.Environment (getEnv)

import Statsbot.Types (MetricSource(..), Metrics(..))

data PostgresMetricSource =
    PSQLSource {
      connectionVar :: Text
    , query :: Query
    }
    deriving Show

instance FromJSON PostgresMetricSource where
    parseJSON = withObject "PSQl Source" $ \v -> do
        connVar <- v .: "connection"
        rawQuery <- v .: "query"
        let query = fromString (rawQuery :: String)
        pure $ PSQLSource {
            connectionVar = connVar,
            query = query
        }

instance MetricSource PostgresMetricSource where
    fetchData PSQLSource {connectionVar, query} = do
        connStr <- encodeUtf8 . T.pack <$> getEnv (T.unpack connectionVar)
        conn <- connectPostgreSQL connStr
        metricRows <- query_ conn query
        pure $ Metrics metricRows
