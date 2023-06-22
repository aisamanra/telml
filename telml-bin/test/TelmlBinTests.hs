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
    result `shouldBe` "<p><em>Hello!</em></p>\n"

  it "uses <p> when no `document` is defined" $ do
    Right result <-
      run
        test
          { program = "One!\n\nTwo!\n"
          }
    result `shouldBe` "<p>One!</p>\n<p>Two!\n</p>\n"

  it "uses `document` if it is defined" $ do
    Right result <-
      run
        test
          { program = "One!\n\nTwo!\n",
            tags =
              "function telml.document(...)\n\
              \  local result = ''\n\
              \  for idx, arg in ipairs({...}) do\n\
              \    result = result .. '- ' .. arg .. '\\n'\n\
              \  end\n\
              \  return result\n\
              \end\n"
          }
    result `shouldBe` "- One!\n- Two!\n\n"
