module Main where

import Protolude
import qualified Data.Text as T
import Statsbot
import System.IO (BufferMode(..), hSetBuffering)

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    args <- getArgs
    reportFile <- maybe (print "You must provide a report .yaml file" *> exitFailure)
                        pure $
                        head args
    let testMode = "--test" `elem` args
    runStatsbot reportFile testMode
