{-# LANGUAGE OverloadedStrings #-}
module Stockfighter.Stock.Quote (
  getQuote
  ) where

import Data.Aeson (FromJSON (..), (.:), (.:?), Value (..))
import Control.Monad (mzero)
import Data.Text (unpack)

import Stockfighter
import Stockfighter.Guts

newtype StockQuote = StockQuote Quote

unwrapQuote :: StockQuote -> Quote
unwrapQuote (StockQuote quote) = quote

instance FromJSON StockQuote where
  parseJSON (Object v) = fmap StockQuote $ Quote <$>
                         v .: "ok" <*>
                         v .: "symbol" <*>
                         v .: "venue" <*>
                         v .:? "bid" <*>
                         v .:? "ask" <*>
                         v .: "bidSize" <*>
                         v .: "askSize" <*>
                         v .: "bidDepth" <*>
                         v .: "askDepth" <*>
                         v .: "last" <*>
                         v .: "lastSize" <*>
                         v .: "lastTrade" <*>
                         v .: "quoteTime"
  parseJSON _ = mzero

getQuote :: Venue -> Stock -> IO (Either String Quote)
getQuote (Venue v) (Stock s) =
  fmap (fmap unwrapQuote)
  (doGet $ "https://api.stockfighter.io/ob/api/venues/" ++ unpack v ++
   "/stocks/" ++ unpack s ++ "/quote")
