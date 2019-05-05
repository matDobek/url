import Test.Hspec
import Test.QuickCheck
import Control.Lens hiding (element)

import Lib
import Internal.Lib
import Types

-- Example

exampleUrl :: Url
exampleUrl = Url {
  _protocol = Http,
  _hostname  = "example.com",
  _path      = "/about",
  _params   = [
    ("user_id", "1"),
    ("auth", "true")
  ]
}

makeLenses ''Url

-- Tests

main = do
  hspec $ do
    describe "isSafe" $ do
      it "returns false when unsafe" $ do
        let subject = set protocol Http exampleUrl
        isSafe(subject) `shouldBe` False

      it "returns true when safe" $ do
        let subject = set protocol Https exampleUrl
        isSafe(subject) `shouldBe` True

    describe "urlToString" $ do
      it "returns stringified HTTP url" $ do
        let emptyParams = []
        let subject = set params emptyParams exampleUrl
        urlToString(subject) `shouldBe` "http://example.com/about"

      it "returns stringified HTTP url" $ do
        urlToString(exampleUrl) `shouldBe` "http://example.com/about?user_id=1&auth=true"

      it "returns stringified HTTPS url" $ do
        let subject = set protocol Https exampleUrl
        urlToString(subject) `shouldBe` "https://example.com/about?user_id=1&auth=true"

    describe "stringToUrl" $ do
      it "it returns Nothing when protocl is not supported" $ do
        let urlAsString = "tcp://example.com"
        stringToUrl(urlAsString) `shouldBe` Nothing

      context "when path is empty" $ do
        it "converts string to an URL data type" $ do
          let urlAsString = "http://example.com"
          let resultUrl = Just Url {
            _protocol = Http,
            _hostname  = "example.com",
            _path      = "/",
            _params   = []
          }
          stringToUrl(urlAsString) `shouldBe` resultUrl

      context "when path is present" $ do
        it "converts string to an URL data type" $ do
          let urlAsString = "http://example.com/foo/bar"
          let resultUrl = Just Url {
            _protocol = Http,
            _hostname  = "example.com",
            _path      = "/foo/bar",
            _params   = []
          }
          stringToUrl(urlAsString) `shouldBe` resultUrl

      context "when params are present" $ do
        it "converts string to an URL data type" $ do
          let urlAsString = "http://example.com/foo/bar?foo=1&bar=2"
          let resultUrl = Just Url {
            _protocol = Http,
            _hostname  = "example.com",
            _path      = "/foo/bar",
            _params   = [ ("foo", "1"), ("bar", "2") ]
          }
          stringToUrl(urlAsString) `shouldBe` resultUrl
