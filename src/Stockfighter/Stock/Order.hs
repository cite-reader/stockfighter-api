{-# LANGUAGE OverloadedStrings #-}
module Stockfighter.Stock.Order (
  Order (..),
  Result (..),
  Fill (..),  
  placeOrder
  ) where

import Data.Time.LocalTime (ZonedTime)
import Data.Vector (Vector)
import Data.Text (unpack)
import Data.Aeson (ToJSON (..), FromJSON (..), Value (..), (.:))
import qualified Data.HashMap.Strict as M
import Data.Scientific (scientific)
import Control.Monad (mzero)
import Control.Applicative ((<|>))

import Stockfighter
import Stockfighter.Guts (doPost)

data Order = Order {
  account :: !Account,
  venue :: !Venue,
  stock :: !Stock,
  price :: {-# UNPACK #-} !Word,
  qty :: {-# UNPACK #-} !Word,
  direction :: !Direction,
  orderType :: !OrderType
  } deriving Show

instance ToJSON Order where
  toJSON o = Object $ M.fromList [
    ("account", String . getAccount $ account o),
    ("venue", String . getVenue $ venue o),
    ("stock", String . getStock $ stock o),
    ("price", Number . asScientific $ price o),
    ("qty", Number . asScientific $ qty o),
    ("direction", toJSON (direction o)),
    ("orderType", toJSON (orderType o))
    ]
    where
      asScientific n = scientific (toInteger n) 0

data Result = Result {
  ok :: !Bool,
  rSymbol :: !Stock,
  rVenue :: !Venue,
  rDirection :: !Direction,
  outstandingQty :: {-# UNPACK #-} !Word,
  rPrice :: {-# UNPACK #-} !Word,
  rType :: !OrderType,
  identity :: {-# UNPACK #-} !Word,
  rAccount :: !Account,
  timestamp :: !ZonedTime, -- ^ When the exchange received the order
  fills :: !(Vector Fill),
  totalFilled :: {-# UNPACK #-} !Word,
  open :: !Bool
  } deriving Show

instance FromJSON Result where
  parseJSON (Object o) = Result <$>
                         o .: "ok" <*>
                         fmap Stock (o .: "symbol") <*>
                         fmap Venue (o .: "venue") <*>
                         o .: "direction" <*>
                         o .: "qty" <*>
                         o .: "price" <*>
                         (o .: "type" <|> o .: "orderType") <*>
                         o .: "id" <*>
                         fmap Account (o .: "account") <*>
                         o .: "ts" <*>
                         o .: "fills" <*>
                         o .: "totalFilled" <*>
                         o .: "open"
  parseJSON _ = mzero

data Fill = Fill {
  fillPrice :: {-# UNPACK #-} !Word,
  fillQty :: {-# UNPACK #-} !Word,
  ts :: !ZonedTime
  } deriving Show

instance FromJSON Fill where
  parseJSON (Object o) = Fill <$>
                         o .: "price" <*>
                         o .: "qty" <*>
                         o .: "ts"
  parseJSON _ = mzero

placeOrder :: Key -> Order -> IO (Either String Result)
placeOrder key order = doPost url key order
  where
    url = "https://api.stockfighter.io/ob/api/venues/" ++ ven ++ "/stocks/" ++
          sto ++ "/orders"
    ven = case venue order of
      Venue v -> unpack v
    sto = case stock order of
      Stock s -> unpack s
