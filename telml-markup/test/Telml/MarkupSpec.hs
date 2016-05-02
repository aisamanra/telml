{-# LANGUAGE OverloadedStrings #-}

module Telml.MarkupSpec (main, spec) where

import Control.Monad ((>=>))
import Data.TeLML
import Data.TeLML.Markup
import Text.Blaze.Renderer.String (renderMarkup)

import Test.Hspec

main :: IO ()
main = hspec spec

doRender :: String -> Either String String
doRender = parse >=> render >=> return . renderMarkup

spec :: Spec
spec = do
  describe "render" $ do
    it "should emphasize" $ do
      doRender "\\em{foo}" `shouldBe` Right "<p><em>foo</em></p>"
    it "should embolden" $ do
      doRender "\\strong{foo}" `shouldBe` Right "<p><strong>foo</strong></p>"
    it "should lis" $ do
      doRender "\\ul{\\li{one}\\li{two}}" `shouldBe`
        Right "<p><ul><li>one</li><li>two</li></ul></p>"
