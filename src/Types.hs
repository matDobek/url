module Types
  (
    Url(..)
  , Protocol(..)
  ) where

data Protocol = Http | Https
  deriving (Show, Eq)

data Url = Url {
  _protocol :: Protocol,
  _hostname :: String,
  _path     :: String,
  _params   :: [(String, String)]
} deriving (Show, Eq)
