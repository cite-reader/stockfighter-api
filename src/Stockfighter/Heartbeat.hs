{-# LANGUAGE OverloadedStrings #-}

module Stockfighter.Heartbeat (
  Result (..),
  heartbeat
  ) where

import Data.Text (Text)
import Data.Aeson (FromJSON (..), (.:), Value (..), decode)
import Control.Lens ((^.))
import Network.Wreq
import Control.Monad (mzero)

data Result = Result {
  ok :: !Bool,
  error :: !Text
  } deriving (Eq, Show)

instance FromJSON Result where
  parseJSON (Object v) = Result <$>
                         v .: "ok" <*>
                         v .: "error"
  parseJSON _ = mzero

heartbeat :: IO (Maybe Result)
heartbeat = decode . (^. responseBody) <$>
            get "https://api.stockfighter.io/ob/api/heartbeat"
