{-# LANGUAGE OverloadedStrings #-}
module Stockfighter.Venue.Heartbeat (
  Result(..),
  heartbeat
) where

import Data.Text (unpack)
import Data.Aeson (FromJSON (..), (.:), Value (..))
import Control.Monad (mzero)

import Stockfighter (Venue (..))
import Stockfighter.Guts (doGet)

data Result = Result {
  ok :: !Bool,
  venue :: !Venue
  } deriving (Eq, Show)

instance FromJSON Result where
  parseJSON (Object v) = Result <$>
                         v .: "ok" <*>
                         (Venue <$> v .: "venue")
  parseJSON _ = mzero

heartbeat :: Venue -> IO (Either String Result)
heartbeat (Venue v) = doGet $ "https://api.stockfighter.io/ob/api/venues/"
                      ++ unpack v ++ "/heartbeat"
