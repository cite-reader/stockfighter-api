{-# LANGUAGE OverloadedStrings #-}
module Stockfighter.Venue.Stocks (
  Result (..),
  Symbol (..),
  getStocks
  ) where

import Data.Vector (Vector)
import Data.Text (Text, unpack)
import Data.Aeson (FromJSON (..), (.:), Value (..))
import Control.Monad (mzero)

import Stockfighter (Venue (Venue), Stock (Stock))
import Stockfighter.Guts (doGet)

data Result = Result {
  ok :: !Bool,
  symbols :: !(Vector Symbol)
  } deriving (Eq, Show)

data Symbol = Symbol {
  name :: !Text,
  symbol :: !Stock
  } deriving (Eq, Show)

instance FromJSON Symbol where
  parseJSON (Object v) = Symbol <$>
                         v .: "name" <*>
                         fmap Stock (v .: "symbol")
  parseJSON _ = mzero

instance FromJSON Result where
  parseJSON (Object v) = Result <$>
                         v .: "ok" <*>
                         (v .: "symbols" >>= parseJSON)
  parseJSON _ = mzero


getStocks :: Venue -> IO (Either String Result)
getStocks (Venue v) = doGet $ "https://api.stockfighter.io/ob/api/venues/"
                      ++ unpack v ++ "/stocks"
