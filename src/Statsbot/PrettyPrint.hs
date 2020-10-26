module Statsbot.PrettyPrint where

import Protolude

import qualified Data.Text as T

import Statsbot.Types

-- | Output a list of `ReportRow`s in a nice fixed-width table, using Slack's supported markdown
prettySlackReport ::
    [ReportRow]
    -> Text
prettySlackReport rows = T.intercalate "\n" prettyRows
    where
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
        pad len str = let
            extraSpaces = len - T.length str
            in str <> T.replicate extraSpaces " "

        bold str = "*" <> str <> "*"
        helpLink str = "<"<> str <>"|Full report>"

-- | Determine the widths of each column
widths ::
    [ReportRow]
    -> (Int, Int, Int, Int)
widths =
    tupelize . map ((+ 4) . maximum) . transpose . map widthVector
    where
        widthVector (ReportRow (Title t) (Link l) d p) =
            [
              T.length t
            , T.length l
            , T.length . T.pack $ show d
            , T.length . T.pack $ show p
            ]

        tupelize [a,b,c,d] = (a,b,c,d)
