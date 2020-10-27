module Statsbot.Types where

import Protolude hiding (show)
import Prelude (Show(..))

import Data.Aeson
import Data.Time.Clock (UTCTime)

-- | Encapsulates how to retrive raw data from a storage system & convert it into a
-- | `Metrics`.
class MetricSource a where
    fetchData :: a -> IO Metrics

-- | A heterogeneous wrapper for unevaluated metrics queries, regardless of the data source
data PendingMetrics = forall a. (MetricSource a, FromJSON a, Show a) => PendingMetrics a
instance Show PendingMetrics where
    show (PendingMetrics src) = show src

newtype Metrics = Metrics [(Float, UTCTime)]
newtype Title = Title Text
    deriving newtype (FromJSON, Show)
newtype Link = Link Text
    deriving newtype (FromJSON, Show)

-- | The configuration for a single row in the report
data Row = Row Title Link PendingMetrics
    deriving Show

data ReportRow =
    ReportRow {
      title :: Title
    , link :: Link
    , daily :: Float
    , historicalMedian :: Float
    }
