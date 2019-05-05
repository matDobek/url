module Lib
  (
    isSafe
  , urlToString
  , stringToUrl
  ) where

import Control.Lens hiding (element)
import Data.List
import Data.List.Split
import Data.Maybe

import Types
import Internal.Lib

makeLenses ''Url

isSafe :: Url -> Bool
isSafe u = _protocol u == safeProtocol

urlToString :: Url -> String
urlToString u =
  protocolToString (_protocol u) ++
  "://" ++
  _hostname u ++
  _path u ++
  paramsToString u

stringToUrl :: String -> Maybe Url
stringToUrl s
  | protocolPart == Nothing = Nothing
  | otherwise = Just Url {
     _protocol = fromJust protocolPart,
     _hostname = hostnamePart,
     _path     = pathPart,
     _params   = paramsPart
   }

  where
    fetchProtocol = (stringToProtocol . head . splitOn "://") s
    fetchHostname = (head . splitOn "/" . last . splitOn "://") s
    --splittedString = splitOn "://" s
    --protocolPart = stringToProtocol $ head splittedString
    --uri = last splittedString
    --hostnamePart = head $ splitOn "/" uri
    --pathPart = head $ splitOn "?" ('/' : (intercalate "/" $ tail $ splitOn "/" uri))
    --paramsPart
      -- | length splittedUri < 2 = []
      -- | length splittedParams < 2 = []
      -- | otherwise = map (\s -> fromJust $ paramTouple $ splitOn "=" s ) $ splittedParams
      --where
        --splittedUri = splitOn "?" uri
        --splittedParams = splitOn "&" $ last $ splittedUri
    --paramTouple [a,b] = Just (a, b)
    --paramTouple _ = Nothing
