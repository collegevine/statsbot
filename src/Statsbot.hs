module Statsbot (
    produceRows
)
where

import Protolude

import Statsbot.Config (ConfigErrorMessage(..), loadRowConfigurations)
import Statsbot.Source (EvaluatedMetrics(..), evaluateSource)
import Statsbot.Types

produceRows ::
    FilePath
    -> IO [ReportRow]
produceRows configFile = do
    loaderResults <- loadRowConfigurations configFile
    rows <- case loaderResults of
                Right rows -> pure rows
                Left (ConfigLoadError err) -> putStrLn err *>  pure []
    mapM toReportRow rows
    where
        toReportRow (Row t l m) = do
            evaluated <- evaluateSource m
            pure $ ReportRow {
                  title = t
                , link = l
                , daily = todayValue evaluated
                , historicalMedian = median evaluated
                }

