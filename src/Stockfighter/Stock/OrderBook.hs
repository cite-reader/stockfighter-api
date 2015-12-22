{-# LANGUAGE OverloadedStrings #-}
module Stockfighter.Stock.OrderBook (
  Result (..),
  Bid (..),
  getOrderBook
  ) where

import Data.Text (unpack)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Aeson (FromJSON (..), (.:), Value (..))
import Data.Time.Clock (UTCTime)
import Control.Monad (mzero)
import qualified Data.HashMap.Strict as M

import Stockfighter (Venue (Venue), Stock (Stock))
import Stockfighter.Guts (doGet)

data Result = Result {
  ok :: !Bool,
  venue :: !Venue,
  symbol :: !Stock,
  bids :: !(Vector Bid), -- ^ These are returned in priority order: earlier
                        --   elements execute first, assuming price
                        -- compatibility
  asks :: !(Vector Bid),
  ts :: !UTCTime
  } deriving (Eq, Show)

data Bid = Bid {
  price :: {-# UNPACK #-} !Word, -- ^ in cents
  qty :: {-# UNPACK #-} !Word,
  isBuy :: !Bool
  } deriving (Eq, Show)

instance FromJSON Result where
  parseJSON (Object v) =
    Result <$>
      v .: "ok" <*>
      v .: "venue" <*>
      v .: "symbol" <*>
      v .: "bids" <*>
      -- "asks" is sometimes `null`. Fun times!
      -- There's no side-channel for tracking these things (unless I
      -- unsafePerformIO into an IORef or something), so flatten that case
      -- into an empty vector.
      (case M.lookup "asks" v of
          Nothing   -> pure V.empty
          Just Null -> pure V.empty
          Just as   -> parseJSON as) <*>
      v .: "ts"
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
