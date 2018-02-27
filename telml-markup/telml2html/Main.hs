module Main where

import           Control.Monad ((>=>))
import qualified Data.TeLML as TeLML
import qualified Data.TeLML.Markup as TeLML
import qualified Text.Blaze.Renderer.String as B
import qualified System.Exit as Sys

doRender :: String -> Either String String
doRender = TeLML.parse >=> TeLML.render >=> return . B.renderMarkup

main :: IO ()
main = do
  cs <- getContents
  case doRender cs of
    Left err -> Sys.die err
    Right x  -> putStrLn x
