module Main where

import Control.Monad ((>=>))
import qualified Data.TeLML as TeLML
import qualified Data.TeLML.Markup as TeLML
import qualified System.Console.GetOpt as Opt
import qualified System.Environment as Env
import qualified System.Exit as Sys
import qualified Text.Blaze.Renderer.String as B

data Options = Options
  { optInput :: Maybe FilePath,
    optOutput :: Maybe FilePath
  }

defaultOptions :: Options
defaultOptions =
  Options
    { optInput = Nothing,
      optOutput = Nothing
    }

options :: [Opt.OptDescr (Options -> Options)]
options =
  [ Opt.Option
      ['i']
      ["input"]
      (Opt.ReqArg (\path opt -> opt {optInput = Just path}) "file")
      "Read input from this file",
    Opt.Option
      ['o']
      ["output"]
      (Opt.ReqArg (\path opt -> opt {optOutput = Just path}) "file")
      "Read input from this file"
  ]

doRender :: String -> Either String String
doRender = TeLML.parse >=> TeLML.render >=> return . B.renderMarkup

runPipeline :: Maybe FilePath -> Maybe FilePath -> IO ()
runPipeline inF outF = do
  cs <- case inF of
    Nothing -> getContents
    Just f -> readFile f
  case doRender cs of
    Left err -> Sys.die err
    Right rs -> case outF of
      Nothing -> putStrLn rs
      Just f -> writeFile f rs

main :: IO ()
main = do
  opts <- Opt.getOpt Opt.Permute options `fmap` Env.getArgs
  case opts of
    (fs, [], []) -> do
      let Options
            { optInput = inF,
              optOutput = outF
            } = foldr id defaultOptions fs
      runPipeline inF outF
    (_, _, _) -> do
      Sys.die (Opt.usageInfo "telml2html" options)
