{-# LANGUAGE OverloadedStrings #-}
module Stockfighter.Guts (
  doGet,
  doKeyedGet,
  doPost,
  doDelete,
  websocketClient,
  ) where

import Data.Function ((&))
import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode)
import Network.Wreq (get, getWith, responseBody, postWith, deleteWith, defaults,
                     header)
import Control.Lens ((^.), (.~))
import Wuss (runSecureClient)
import Network.WebSockets
import Control.Monad (forever)
import Control.Exception
import System.Posix.Syslog

import Stockfighter

-- | Performs a GET request to the given URL.
--
--   If the JSON response from the API cannot be decoded, the error message from
--   Aeson is returned in the 'Left'.
doGet :: FromJSON a => String -> IO (Either String a)
doGet url = eitherDecode . (^. responseBody) <$> get url

doKeyedGet :: FromJSON r => Key -> String -> IO (Either String r)
doKeyedGet (Key k) url = eitherDecode . (^. responseBody) <$>
                         getWith (defaults &
                                  header "X-Starfighter-Authorization" .~ [k])
                         url

doPost :: (ToJSON a, FromJSON r) => String -> Key -> a -> IO (Either String r)
doPost url (Key k) body = eitherDecode . (^. responseBody) <$>
                          postWith (defaults &
                                    header "X-Starfighter-Authorization" .~ [k])
                          url
                          (encode body)

doDelete :: FromJSON r => Key -> String -> IO (Either String r)
doDelete (Key k) url = eitherDecode . (^. responseBody) <$>
                       deleteWith (defaults &
                                   header "X-Starfighter-Authorization" .~ [k])
                       url

websocketClient :: FromJSON a => String -> (Either String a -> IO r) -> IO b
websocketClient path act = try run >>= handler
  where
    run = runSecureClient "api.stockfighter.io" 443 path client
    client conn = forever (eitherDecode <$> receiveData conn >>= act)
    handler (Left exn@(CloseRequest code msg))
      | code == 1000 = do
        syslog Notice $ "A websocket was torn down with message " ++ show msg
        websocketClient path act
      | otherwise = do
        syslog Error $
          "A websocket was unexpectedly torn down with code "
          ++ show code ++ " and message " ++ show msg
        throwIO exn
    handler (Left exn) = do
      syslog Warning $
        "A websocket unexpectedly closed: " ++ displayException exn ++
        " (retrying)"
      websocketClient path act
    handler (Right _) = error "The \"impossible\" happened: a loop returned"
