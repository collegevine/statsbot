module Statsbot.PrettyPrint where

import Protolude

import qualified Data.Text as T

import Statsbot.Types

-- | Output a list of `ReportRow`s in a nice fixed-width table, using Slack's supported markdown
prettySlackReport ::
    Text
    -> Int
    -> [ReportRow]
    -> Text
prettySlackReport title lookback rows = T.intercalate "\n" (header:prettyRows)
    where
        header = reportHeader rowWidths lookback
        prettyRows = prettySlackRow rowWidths <$> rows
        rowWidths = widths rows

-- | Transforms a report row into:
--
prettySlackRow ::
    (Int, Int, Int, Int)
    -> ReportRow
    -> Text
prettySlackRow (col1, col2, col3, col4) (ReportRow (Title t) (Link l) d p) =
    pad col1 (bold t) <> pad col2 (T.pack $ show d) <> pad col3 (T.pack $ show p) <> helpLink l
    where
        bold str = "*" <> str <> "*"
        helpLink str = "<"<> str <>"|Full report>"

reportHeader ::
    (Int, Int, Int, Int)
    -> Int
    -> Text
reportHeader (col1, col2, col3, col4) daysBack =
    pad col1 "Metric" <> pad col2 "Yesterday" <> pad col3 (pastColumnName daysBack) <> pad col4 "Details"

pastColumnName ::
    Int
    -> Text
pastColumnName daysBack = "Past " <> T.pack (show daysBack) <> "-day median"

pad :: Int -> Text -> Text
pad len str = let
    extraSpaces = len - T.length str
    in str <> T.replicate extraSpaces " "

-- | Determine the widths of each column
widths ::
    [ReportRow]
    -> (Int, Int, Int, Int)
widths =
    tupelize . map ((+ 6) . maximum) . transpose . map widthVector
    where
        widthVector (ReportRow (Title t) (Link l) d p) =
            [
              T.length t
            , max (T.length "Yesterday") (T.length . T.pack $ show d)
            , max (T.length $ pastColumnName 30) (T.length . T.pack $ show p)
            , T.length l
            ]

        tupelize [a,b,c,d] = (a,b,c,d)
