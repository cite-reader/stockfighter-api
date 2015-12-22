{-# LANGUAGE OverloadedStrings #-}
module Stockfighter.Stock.Order.Cancel (
  Result (..),
  Fill (..),
  cancelOrder
  ) where

import Data.Time.Clock (UTCTime)
import Data.Aeson (FromJSON (..), (.:), Value (..))
import Control.Monad (mzero)
import Data.Vector (Vector)
import Data.Text (unpack)

import Stockfighter
import Stockfighter.Guts

data Result = Result {
  ok :: !Bool,
  symbol :: !Stock,
  venue :: !Venue,
  direction :: !Direction,
  originalQty :: {-# UNPACK #-} !Word,
  qty :: {-# UNPACK #-} !Word, -- We expect this to be 0 for canceled orders
  price :: {-# UNPACK #-} !Word,
  orderType :: !OrderType,
  identity :: {-# UNPACK #-} !Word,
  account :: !Account,
  ts :: !UTCTime,
  fills :: !(Vector Fill),
  totalFilled :: {-# UNPACK #-} !Word,
  open :: !Bool
  } deriving Show

data Fill = Fill {
  fillPrice :: {-# UNPACK #-} !Word,
  quantity :: {-# UNPACK #-} !Word,
  time :: !UTCTime
  } deriving Show

instance FromJSON Result where
  parseJSON (Object o) = Result <$>
                         o .: "ok" <*>
                         o .: "symbol" <*>
                         o .: "venue" <*>
                         o .: "direction" <*>
                         o .: "originalQty" <*>
                         o .: "qty" <*>
                         o .: "price" <*>
                         o .: "orderType" <*>
                         o .: "id" <*>
                         o .: "account" <*>
                         o .: "ts" <*>
                         o .: "fills" <*>
                         o .: "totalFilled" <*>
                         o .: "open"
  parseJSON _ = mzero

instance FromJSON Fill where
  parseJSON (Object o) = Fill <$>
                         o .: "price" <*>
                         o .: "qty" <*>
                         o .: "ts"
  parseJSON _ = mzero

cancelOrder :: Key -> Venue -> Stock -> Word -> IO (Either String Result)
cancelOrder key (Venue v) (Stock s) order =
  doDelete key $
  "https://api.stockfighter.io/ob/api/venues/" ++ unpack v ++ "/stocks/" ++
  unpack s ++ "/orders/" ++ show order
