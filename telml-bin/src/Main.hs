{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8 as BS
import qualified Data.TeLML as TeLML
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified HsLua.Core as Lua
import qualified System.Console.GetOpt as Opt
import qualified System.Environment as Sys

data Options = Options
  { optInputFile :: Maybe String,
    optUseDefaultTags :: Bool,
    optTagFile :: Maybe String
  }
  deriving (Eq, Show)

opts :: [Opt.OptDescr (Options -> Options)]
opts =
  [ Opt.Option
      ['n']
      ["no-default-tags"]
      (Opt.NoArg (\o -> o {optUseDefaultTags = False}))
      "Do not include any default tags",
    Opt.Option
      ['t']
      ["tags"]
      (Opt.ReqArg (\f o -> o {optTagFile = Just f}) "[file]")
      "The file of tag definitions to use"
  ]

parseOpts :: IO Options
parseOpts = do
  args <- Sys.getArgs
  let def =
        Options
          { optInputFile = Nothing,
            optUseDefaultTags = True,
            optTagFile = Nothing
          }
  case Opt.getOpt Opt.Permute opts args of
    (flags, [], []) ->
      return (foldl (flip id) def flags)
    (flags, [input], []) ->
      return (foldl (flip id) def flags) {optInputFile = Just input}
    (_, _, errors) ->
      error (unlines errors)

main :: IO ()
main = do
  options <- parseOpts
  telmlSource <- case optInputFile options of
    Nothing -> getContents
    Just f -> readFile f
  let telml = case TeLML.parse telmlSource of
        Right str -> str
        Left err -> error err
  luaSource <- case optTagFile options of
    Nothing -> return ""
    Just f -> BS.readFile f
  result <- Lua.run (luaMain luaSource telml)
  print result
  return ()

luaMain :: BS.ByteString -> TeLML.Document -> Lua.LuaE Lua.Exception Text.Text
luaMain luaSource doc = do
  Lua.newtable
  Lua.setglobal "telml" :: Lua.LuaE Lua.Exception ()
  _ <- Lua.dostring luaSource
  telml <- Lua.getglobal "telml"
  if telml /= Lua.TypeTable
    then Lua.liftIO (putStrLn "wrong type")
    else return ()
  handleDoc doc

standardTags :: Text.Text -> [Text.Text] -> Maybe Text.Text
standardTags n ps =
  case (n, ps) of
    ("em", [r]) -> Just ("<em>" <> r <> "</em>")
    ("strong", [r]) -> Just ("<strong>" <> r <> "</strong>")
    ("li", [r]) -> Just ("<li>" <> r <> "</li>")
    _ -> Nothing

handleDoc :: TeLML.Document -> Lua.LuaE Lua.Exception Text.Text
handleDoc = fmap mconcat . sequence . map handleFrag

handleFrag :: TeLML.Fragment -> Lua.LuaE Lua.Exception Text.Text
handleFrag (TeLML.TextFrag text) = return text
handleFrag (TeLML.TagFrag tag) = handleTag tag

handleTag :: TeLML.Tag -> Lua.LuaE Lua.Exception Text.Text
handleTag (TeLML.Tag n ps) = do
  ps' <- mapM handleDoc ps
  Lua.pushstring (Text.encodeUtf8 n)
  typ <- Lua.gettable 1
  case typ of
    Lua.TypeNil -> case standardTags n ps' of
      Just r -> return r
      Nothing -> error ("No such tag " ++ show n)
    Lua.TypeFunction -> do
      mapM_ (Lua.pushstring . Text.encodeUtf8) ps'
      Lua.call (Lua.NumArgs (fromIntegral (length ps'))) 1
      result <- Lua.tostring 2
      case result of
        Nothing -> error "expected string"
        Just r -> return (Text.decodeUtf8 r)
    _ -> error ("Expected function, not " ++ show typ)
