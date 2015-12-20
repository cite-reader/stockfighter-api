{-# LANGUAGE OverloadedStrings, LambdaCase #-}
module Stockfighter.Streaming.Quotes (
  Quote (..),
  foldQuotes,
  foldStockQuotes
  ) where

import Data.Time.LocalTime (ZonedTime)
import Data.Aeson (FromJSON (..), (.:), Value (..))
import Control.Monad (mzero)
import Data.Text (unpack)

import Stockfighter
import Stockfighter.Guts

data Quote = Quote {
  ok :: !Bool,
  symbol :: !Stock,
  venue :: !Venue,
  bid :: {-# UNPACK #-} !Word,
  ask :: {-# UNPACK #-} !Word,
  bidSize :: {-# UNPACK #-} !Word,
  askSize :: {-# UNPACK #-} !Word,
  bidDepth :: {-# UNPACK #-} !Word,
  askDepth :: {-# UNPACK #-} !Word,
  last :: {-# UNPACK #-} !Word,
  lastSize :: {-# UNPACK #-} !Word,
  lastTrade :: !ZonedTime,
  quoteTime :: !ZonedTime
  } deriving Show

instance FromJSON Quote where
  parseJSON (Object o) = do
    q <- o .: "quote"
    Quote <$>
      o .: "ok" <*>
      q .: "symbol" <*>
      q .: "venue" <*>
      q .: "bid" <*>
      q .: "ask" <*>
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
foldQuotes (Account a) (Venue v) action = websocketClient path action
  where
    path = "/ob/api/ws/" ++ unpack a ++ "/venues/" ++ unpack v ++ "/tickertape"

foldStockQuotes :: Account -> Venue -> Stock -> (Either String Quote -> IO r)
                   -> IO a
foldStockQuotes (Account a) (Venue v) (Stock s) act = websocketClient path act
  where
    path = "/ob/api/ws/" ++ unpack a ++ "/venues/" ++ unpack v ++
           "/tickertape/stocks/" ++ unpack s
