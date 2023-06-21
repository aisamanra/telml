{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8 as BS
import Data.TeLML.Driver
import qualified Data.Text as Text
import Test.Hspec

data TestCase = TestCase
  { program :: BS.ByteString,
    useDefault :: Bool,
    tags :: BS.ByteString
  }

test :: TestCase
test =
  TestCase
    { program = "",
      useDefault = True,
      tags = ""
    }

run :: TestCase -> IO (Either Error Text.Text)
run ts = mainWithOpts opts
  where
    opts =
      Options
        { optInputFile = Just (InputFileTest (program ts)),
          optUseDefaultTags = useDefault ts,
          optTagFile = Just (InputFileTest (tags ts))
        }

main :: IO ()
main = hspec $ do
  it "succeeds with basic tags" $ do
    Right result <-
      run
        test
          { program = "\\em{Hello!}"
          }
    result `shouldBe` "<em>Hello!</em>"
  return ()
