{-# LANGUAGE OverloadedStrings #-}
module Stockfighter.Stock.Order.Status (
  Result (..),
  Fill (..),
  getStatus,
  ) where

import Data.Time.LocalTime (ZonedTime)
import Data.Aeson (FromJSON (..), (.:), Value (..))
import Control.Monad (mzero)
import Data.Text (unpack)
import Data.Vector (Vector)

import Stockfighter
import Stockfighter.Guts (doKeyedGet)

data Result = Result {
  ok :: !Bool,
  symbol :: !Stock,
  venue :: !Venue,
  direction :: !Direction,
  originalQty :: {-# UNPACK #-} !Word,
  left :: {-# UNPACK #-} !Word,
  price :: {-# UNPACK #-} !Word,
  orderType :: !OrderType,
  identity :: {-# UNPACK #-} !Word,
  account :: !Account,
  orderPlaced :: !ZonedTime,
  fills :: !(Vector Fill),
  totalFills :: {-# UNPACK #-} !Word,
  open :: !Bool
  } deriving Show

data Fill = Fill {
  fillPrice :: {-# UNPACK #-} !Word,
  qty :: {-# UNPACK #-} !Word,
  time :: {-# UNPACK #-} !ZonedTime
  } deriving Show

instance FromJSON Result where
  parseJSON (Object o) = Result <$>
                         o .: "ok" <*>
                         fmap Stock (o .: "symbol") <*>
                         fmap Venue (o .: "venue") <*>
                         o .: "direction" <*>
                         o .: "originalQty" <*>
                         o .: "qty" <*>
                         o .: "price" <*>
                         o .: "orderType" <*>
                         o .: "id" <*>
                         fmap Account (o .: "account") <*>
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

getStatus :: Key -> Word -> Venue -> Stock -> IO (Either String Result)
getStatus key ident (Venue v) (Stock s) =
  doKeyedGet key $ "https://api.stockfighter.io/ob/api/venues/" ++ unpack v ++
                   "/stocks/" ++ unpack s ++ "/orders/" ++ show ident
