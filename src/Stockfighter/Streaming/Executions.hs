{-# LANGUAGE OverloadedStrings #-}
module Stockfighter.Streaming.Executions (
  Execution (..),
  Order (..),
  Fill (..),
  foldExecutions,
  foldStockExecutions
  ) where

import Control.Monad (mzero)
import Data.Aeson (FromJSON (..), (.:), Value (..))
import Data.Time.LocalTime (ZonedTime)
import Data.Vector (Vector)
import Data.Text (unpack)

import Stockfighter
import Stockfighter.Guts

data Execution = Execution {
  ok :: !Bool,
  account :: !Account,
  venue :: !Venue,
  symbol :: !Stock,
  order :: !Order,
  standingId :: {-# UNPACK #-} !Word,
  incomingId :: {-# UNPACK #-} !Word,
  price :: {-# UNPACK #-} !Word,
  filled :: {-# UNPACK #-} !Word,
  filledAt :: !ZonedTime,
  standingComplete :: !Bool,
  incomingComplete :: !Bool
  } deriving Show


data Order = Order {
  oOk :: !Bool,
  oSymbol :: !Stock,
  oVenue :: !Venue,
  direction :: !Direction,
  originalQty :: {-# UNPACK #-} !Word,
  qty :: {-# UNPACK #-} !Word,
  oPrice :: {-# UNPACK #-} !Word,
  orderType :: !OrderType,
  identity :: {-# UNPACK #-} !Word,
  oAccount :: !Account,
  ts :: !ZonedTime,
  fills :: !(Vector Fill),
  totalFilled :: {-# UNPACK #-} !Word,
  open :: !Bool
  } deriving Show

data Fill = Fill {
  fPrice :: {-# UNPACK #-} !Word,
  fQty :: {-# UNPACK #-} !Word,
  fillTime :: !ZonedTime
  } deriving Show

instance FromJSON Execution where
  parseJSON (Object o) =
    Execution <$>
    o .: "ok" <*>
    o .: "account" <*>
    o .: "venue" <*>
    o .: "symbol" <*>
    o .: "order" <*>
    o .: "standingId" <*>
    o .: "incomingId" <*>
    o .: "price" <*>
    o .: "filled" <*>
    o .: "filledAt" <*>
    o .: "standingComplete" <*>
    o .: "incomingComplete"
  parseJSON _ = mzero

instance FromJSON Order where
  parseJSON (Object o) =
    Order <$>
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
  parseJSON (Object o) =
    Fill <$>
    o .: "price" <*>
    o .: "qty" <*>
    o .: "ts"
  parseJSON _ = mzero

foldExecutions :: Account -> Venue -> (Either String Execution -> IO r) -> IO a
foldExecutions (Account a) (Venue v) act = websocketClient path act
  where
    path = "/ob/api/ws/" ++ unpack a ++ "/venues/" ++ unpack v ++ "/executions"

foldStockExecutions :: Account -> Venue -> Stock ->
                       (Either String Execution -> IO r) -> IO a
foldStockExecutions (Account a) (Venue v) (Stock s) act =
  websocketClient path act
  where
    path = "/ob/api/ws/" ++ unpack a ++ "/venues/" ++ unpack v ++
           "/executions/stocks/" ++ unpack s
