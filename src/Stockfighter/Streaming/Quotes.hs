{-# LANGUAGE OverloadedStrings, LambdaCase #-}
module Stockfighter.Streaming.Quotes (
  foldQuotes,
  foldStockQuotes
  ) where

import Data.Aeson (FromJSON (..), (.:), (.:?), Value (..))
import Control.Monad (mzero)
import Data.Text (unpack)

import Stockfighter
import Stockfighter.Guts

newtype StreamingQuote = Streaming Quote

getQuote :: StreamingQuote -> Quote
getQuote (Streaming quote) = quote

instance FromJSON StreamingQuote where
  parseJSON (Object o) = do
    q <- o .: "quote"
    fmap Streaming $ Quote <$>
      o .: "ok" <*>
      q .: "symbol" <*>
      q .: "venue" <*>
      q .:? "bid" <*>
      q .:? "ask" <*>
      q .: "bidSize" <*>
      q .: "askSize" <*>
      q .: "bidDepth" <*>
      q .: "askDepth" <*>
      q .: "last" <*>
      q .: "lastSize" <*>
      q .: "lastTrade" <*>
      q .: "quoteTime"
  parseJSON _ = mzero

foldQuotes :: Account -> Venue -> (Either String Quote -> IO r) -> IO a
foldQuotes (Account a) (Venue v) action =
  websocketClient path (action . (fmap getQuote))
  where
    path = "/ob/api/ws/" ++ unpack a ++ "/venues/" ++ unpack v ++ "/tickertape"

foldStockQuotes :: Account -> Venue -> Stock -> (Either String Quote -> IO r)
                   -> IO a
foldStockQuotes (Account a) (Venue v) (Stock s) action =
  websocketClient path (action . (fmap getQuote))
  where
    path = "/ob/api/ws/" ++ unpack a ++ "/venues/" ++ unpack v ++
           "/tickertape/stocks/" ++ unpack s
