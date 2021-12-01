module Statsbot (
    runStatsbot
)
where

import Protolude

import Statsbot.Config
import Statsbot.Source (EvaluatedMetrics(..), evaluateSource)
import Statsbot.PrettyPrint (prettySlackReport)
import Statsbot.Publish (publishReport)
import Statsbot.Types

runStatsbot ::
    FilePath
    -> Bool
    -> IO ()
runStatsbot configFilePath testMode = do
    loaderResults <- loadRowConfigurations configFilePath
    config <- case loaderResults of
                Right rows -> pure rows
                Left (ConfigLoadError err) -> putStrLn err *>  exitFailure
    rows <- mapM toReportRow $ rows config
    let body = prettySlackReport (reportTitle config) (historyWindowDays config) rows
    if testMode
        then putStrLn body
        else publishReport (targetURL config) (reportTitle config) body

toReportRow (Row t l m) = do
    evaluated <- evaluateSource m
    pure $ ReportRow {
            title = t
        , link = l
        , daily = todayValue evaluated
        , historicalMedian = median evaluated
        }
