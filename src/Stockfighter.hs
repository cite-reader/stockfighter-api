{-# LANGUAGE OverloadedStrings #-}
module Stockfighter (
  Venue (..), getVenue,
  Stock (..), getStock,
  Account (..), getAccount,
  Direction (..),
  OrderType (..),
  Key (..), getKey
  ) where

import Data.Text (Text)
import Data.ByteString (ByteString)
import Data.Aeson (ToJSON (..), Value (..), FromJSON (..))
import Control.Monad (mzero)

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
