{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.TeLML as TeLML
import Test.Hspec

main :: IO ()
main = hspec $ do
  it "parses tagless text as one fragment" $ do
    TeLML.parse "foo bar baz"
      `shouldBe` Right [TeLML.TextFrag "foo bar baz"]
  it "parses basic tags" $ do
    TeLML.parse "foo \\bar{} baz"
      `shouldBe` Right
        [ TeLML.TextFrag "foo ",
          TeLML.TagFrag (TeLML.Tag "bar" []),
          TeLML.TextFrag " baz"
        ]
  it "parses escaped tags" $ do
    TeLML.parse "foo \\\\bar\\{\\} baz"
      `shouldBe` Right [TeLML.TextFrag "foo \\bar{} baz"]
  it "does not parse bracketless tags" $ do
    TeLML.parse "foo \\bar baz"
      `shouldBe` Left "expected start of block"
