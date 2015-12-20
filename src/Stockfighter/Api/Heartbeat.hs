{-# LANGUAGE OverloadedStrings #-}

module Stockfighter.Api.Heartbeat (
  Result (..),
  heartbeat
  ) where

import Data.Text (Text)
import Data.Aeson (FromJSON (..), (.:), Value (..))
import Control.Monad (mzero)

import Stockfighter.Guts (doGet)

data Result = Result {
  ok :: !Bool,
  error :: !Text
  } deriving (Eq, Show)

instance FromJSON Result where
  parseJSON (Object v) = Result <$>
                         v .: "ok" <*>
                         v .: "error"
  parseJSON _ = mzero

heartbeat :: IO (Either String Result)
heartbeat = doGet "https://api.stockfighter.io/ob/api/heartbeat"
