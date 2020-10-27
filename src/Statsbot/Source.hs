module Statsbot.Source
(
    PostgresMetricSource(..),
    EvaluatedMetrics(..),
    evaluateSource
) where

import Protolude

import Data.Time.Clock (UTCTime)

import Statsbot.Types
import Statsbot.Source.PostgresSQL (PostgresMetricSource(..))

type DataPoint = (Float, UTCTime)

data EvaluatedMetrics = EvaluatedMetrics {
      todayValue :: Float
    , median :: Float
    }

evaluateSource ::
    PendingMetrics
    -> IO EvaluatedMetrics
evaluateSource (PendingMetrics m) = do
    metrics <- fetchData m
    let median = findMedian metrics
        latest = findLatest metrics
    pure $ EvaluatedMetrics {
          todayValue = latest
        , median = median
        }

findMedian ::
    Metrics
    -> Float
findMedian (Metrics rawData) =
    maybe (fromRational notANumber) fst .
        head .
        drop (length rawData `div` 2)  $
        sortBy timestampComparison rawData

findLatest ::
    Metrics
    -> Float
findLatest (Metrics rawData) =
    fst $ maximumBy timestampComparison rawData

timestampComparison ::DataPoint -> DataPoint -> Ordering
timestampComparison = (comparing snd)
