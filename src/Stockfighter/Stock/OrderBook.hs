{-# LANGUAGE OverloadedStrings #-}
module Stockfighter.Stock.OrderBook (
  Result (..),
  Bid (..),
  getOrderBook
  ) where

import Data.Text (unpack)
import Data.Vector (Vector)
import Data.Aeson (FromJSON (..), (.:), Value (..))
import Control.Monad (mzero)

import Stockfighter (Venue (Venue), Stock (Stock))
import Stockfighter.Guts (doGet)

data Result = Result {
  ok :: !Bool,
  venue :: !Venue,
  symbol :: !Stock,
  bids :: !(Vector Bid) -- ^ These are returned in priority order: earlier
                        --   elements execute first, assuming price
                        -- compatibility
  } deriving (Eq, Show)

data Bid = Bid {
  price :: {-# UNPACK #-} !Word, -- ^ in cents
  qty :: {-# UNPACK #-} !Word,
  isBuy :: !Bool
  } deriving (Eq, Show)

instance FromJSON Result where
  parseJSON (Object v) = Result <$>
                         v .: "ok" <*>
                         fmap Venue (v .: "venue") <*>
                         fmap Stock (v .: "symbol") <*>
                         v .: "bids"
  parseJSON _ = mzero

instance FromJSON Bid where
  parseJSON (Object v) = Bid <$>
                         v .: "price" <*>
                         v .: "qty" <*>
                         v .: "isBuy"
  parseJSON _ = mzero

getOrderBook :: Venue -> Stock -> IO (Either String Result)
getOrderBook (Venue v) (Stock s) =
  doGet $ "https://api.stockfighter.io/ob/api/venues/" ++ unpack v ++ "/stocks/"
  ++ unpack s
