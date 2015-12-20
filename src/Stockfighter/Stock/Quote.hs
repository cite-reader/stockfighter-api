{-# LANGUAGE OverloadedStrings #-}
module Stockfighter.Stock.Quote (
  Result (..),
  getQuote
  ) where

import Data.Aeson (FromJSON (..), (.:), Value (..))
import Control.Monad (mzero)
import Data.Text (unpack)

import Stockfighter (Venue (Venue), Stock (Stock))

data Result = Result {
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
  lastTrade :: !DateTime,
  quoteTime :: !DateTime
  } deriving (Eq, Show)

instance FromJSON Result where
  parseJson (Object v) = Result <$>
                         v .: "ok" <*>
                         v .: "symbol" <*>
                         v .: "bid" <*>
                         v .: "ask" <*>
                         v .: "bidSize" <*>
                         v .: "askSize" <*>
                         v .: "bidDepth" <*>
                         v .: "last" <*>
                         v .: "lastSize" <*>
                         v .: "lastTrade" <*>
                         v .: "quoteTime"
  parseJson _ = mzero

getQuote :: Venue -> Stock -> IO (Either String Result)
getQuote (Venue v) (Stock s) =
  doGet $ "https://api.stockfighter.io/ob/api/venues/" ++ unpack v ++
          "/stocks/" ++ unpack s ++ "/quote
