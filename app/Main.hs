module Main where

import Protolude
import qualified Data.Text as T
import Statsbot

main :: IO ()
main = do
    args <- getArgs
    reportFile <- maybe (print "You must provide a report .yaml file" *> exitFailure)
                        pure $
                        head args
    runStatsbot reportFile
