module Stockfighter (
  ApiKey (ApiKey)
  ) where

import Data.Text (Text)

newtype ApiKey = ApiKey Text deriving (Eq, Show)
