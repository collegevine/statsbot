module Statsbot.Publish where

import Protolude
import Prelude(String)

import Statsbot.Types(Title(..))

import Data.Aeson
import qualified Data.Text as T
import Network.HTTP.Client
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types.Header (hContentType)
import Network.HTTP.Types.Status (statusCode)


publishReport ::
    Text -- ^ Webhook URL
    -> Text
    -> Text -- ^ Report body
    -> IO ()
publishReport destination t body = do
    manager <- getManager
    req <- parseRequest $ T.unpack destination
    let slackPost = req {
          method = "POST"
        , requestBody = RequestBodyLBS $ encode payload
        , requestHeaders = [(hContentType, "application/json")]
        }
    response <- httpLbs slackPost manager
    if statusCode (responseStatus response) >= 400
    then (print $ ("ERROR posting to Slack: " <> show response :: String)) *> exitFailure
    else print ("Published report to Slack"::Text)
    where
        getManager
            | T.isPrefixOf "https://" destination = newTlsManager
            | otherwise = newManager defaultManagerSettings

        payload = ReportPayload {title = t, reportBody = body}


data ReportPayload =
    ReportPayload {
      title :: Text
    , reportBody :: Text
    }


-- | See https://api.slack.com/messaging/webhooks for details on the Slack payload format
instance ToJSON ReportPayload where
    toJSON payload = object [
          "text" .=  title payload
        , "blocks" .= toJSON [
            object [
                  "type" .= toJSON ("section"::Text)
                , "text" .= object [
                      "type" .= ("mrkdwn"::Text)
                    , "text" .= reportBody payload
                    ]
                ]
            ]
        ]

