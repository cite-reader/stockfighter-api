{-# LANGUAGE OverloadedStrings #-}
module Stockfighter (
  Venue (..), getVenue,
  Stock (..), getStock,
  Account (..), getAccount,
  Direction (..),
  OrderType (..),
  Key (..), getKey,
  Quote (..),
  TemporalQuote (..)
  ) where

import Data.Text (Text)
import Data.ByteString (ByteString)
import Data.Aeson (ToJSON (..), Value (..), FromJSON (..))
import Control.Monad (mzero)
import Data.Time.Clock (UTCTime)
import Data.Ord (comparing)

newtype Venue = Venue Text deriving (Eq, Show)

getVenue :: Venue -> Text
getVenue (Venue v) = v

instance FromJSON Venue where
  parseJSON (String s) = pure (Venue s)
  parseJSON _ = mzero

newtype Stock = Stock Text deriving (Eq, Show)

getStock :: Stock -> Text
getStock (Stock s) = s

instance FromJSON Stock where
  parseJSON (String s) = pure (Stock s)
  parseJSON _ = mzero

newtype Account = Account Text deriving (Eq, Show)

getAccount :: Account -> Text
getAccount (Account a) = a

instance ToJSON Account where
  toJSON (Account t) = toJSON t

instance FromJSON Account where
  parseJSON (String a) = pure (Account a)
  parseJSON _ = mzero

data Direction = Buy | Sell deriving (Eq, Show)

instance ToJSON Direction where
  toJSON Buy = String "buy"
  toJSON Sell = String "sell"

instance FromJSON Direction where
  parseJSON (String s)
     | s == "buy" = pure Buy
     | s == "sell" = pure Sell
     | otherwise = mzero
  parseJSON _ = mzero

data OrderType = Limit | Market | FillOrKill | ImmediateOrCancel
               deriving (Eq, Show)

instance ToJSON OrderType where
  toJSON Limit = String "limit"
  toJSON Market = String "market"
  toJSON FillOrKill = String "fill-or-kill"
  toJSON ImmediateOrCancel = String "immediate-or-cancel"

instance FromJSON OrderType where
  parseJSON (String s)
    | s == "limit" = pure Limit
    | s == "market" = pure Market
    | s == "fill-or-kill" = pure FillOrKill
    | s == "immediate-or-cancel" = pure ImmediateOrCancel
    | otherwise = mzero
  parseJSON _ = mzero

newtype Key = Key ByteString deriving (Eq, Show)

getKey :: Key -> ByteString
getKey (Key k) = k

data Quote = Quote {
  ok :: !Bool,
  symbol :: !Stock,
  venue :: !Venue,
  bid :: !(Maybe Word),
  ask :: !(Maybe Word),
  bidSize :: {-# UNPACK #-} !Word,
  askSize :: {-# UNPACK #-} !Word,
  bidDepth :: {-# UNPACK #-} !Word,
  askDepth :: {-# UNPACK #-} !Word,
  last :: {-# UNPACK #-} !Word,
  lastSize :: {-# UNPACK #-} !Word,
  lastTrade :: !UTCTime,
  quoteTime :: !UTCTime
  } deriving (Eq, Show)

-- | It would be nice to place 'Quote's in something like an LVIsh LVar, but
--   the only ordering we care about is /temporal/ order on the exchange\'s
--   world-line. Implementing 'Ord' on 'Quote' directly would cause it to
--   disagree with the natural 'Eq', and crippling the base type\'s 'Eq' is...
--   dubious. This newtype explicitly projects us into the world where we only
--   care about the time the quote is generated.
newtype TemporalQuote = Temporal Quote deriving Show

instance Eq TemporalQuote where
  Temporal q1 == Temporal q2 = quoteTime q1 == quoteTime q2

instance Ord TemporalQuote where
  compare (Temporal q1) (Temporal q2) = comparing quoteTime q1 q2
