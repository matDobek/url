module Internal.Lib
  (
    protocolToString
  , stringToProtocol
  , paramsToString
  , safeProtocol
  ) where

import Control.Lens hiding (element)
import Data.List.Split
import Data.Maybe

import Types

makeLenses ''Url

protocolToString :: Protocol -> String
protocolToString Http = "http"
protocolToString Https = "https"

stringToProtocol :: String -> Maybe Protocol
stringToProtocol "http" = Just Http
stringToProtocol "https" = Just Https
stringToProtocol _ = Nothing

paramsToString :: Url -> String
paramsToString u = foldl joinString "" (view params u)
 where
   joinString [] (a, b) = "?" ++ a ++ "=" ++ b
   joinString acc (a, b) = acc ++ "&" ++ a ++ "=" ++ b

safeProtocol :: Protocol
safeProtocol = Https
