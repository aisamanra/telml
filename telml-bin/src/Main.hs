{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Control.Exception.Base as Exn
import qualified Data.ByteString.Char8 as BS
import qualified Data.TeLML as TeLML
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
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

data Error
  = LuaError Lua.Exception
  | TeLMLError Exn.SomeException
  deriving (Show)

instance Exn.Exception Error where
  displayException (LuaError err) = Exn.displayException err
  displayException (TeLMLError err) = Exn.displayException err

instance Lua.LuaError Error where
  popException = do
    err <- Lua.changeErrorType Lua.popException
    return (LuaError err)

  pushException (LuaError err) = do
    Lua.changeErrorType (Lua.pushException err)
  pushException (TeLMLError err) = do
    let str = BS.pack (Exn.displayException err)
    Lua.pushstring str

  luaException = LuaError . Lua.luaException

type LuaM r = Lua.LuaE Error r

throw :: Exn.Exception e => e -> LuaM a
throw =
  Lua.liftIO . Exn.throwIO . TeLMLError . Exn.toException

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
  result <- Lua.runEither (luaMain luaSource telml)
  case result of
    Right msg -> Text.putStr msg
    Left err -> putStrLn (Exn.displayException err)
  return ()

luaMain :: BS.ByteString -> TeLML.Document -> LuaM Text.Text
luaMain luaSource doc = do
  Lua.openbase
  Lua.pop 1

  Lua.newtable
  Lua.setglobal "telml"
  _ <- Lua.dostring luaSource
  telml <- Lua.getglobal "telml"
  if telml /= Lua.TypeTable
    then Lua.liftIO (putStrLn "wrong type")
    else return ()
  handleDoc doc

data BuiltinArityMismatch = BuiltinArityMismatch
  { bamExpected :: Int,
    bamProvided :: Int,
    bamTagName :: Text.Text
  }
  deriving (Show)

instance Exn.Exception BuiltinArityMismatch where
  displayException bam =
    concat
      [ "Tag `\\",
        Text.unpack (bamTagName bam),
        "`: expected ",
        show (bamExpected bam),
        " argument(s), got ",
        show (bamProvided bam)
      ]

data NoSuchTag = NoSuchTag {nstName :: Text.Text} deriving (Show)

instance Exn.Exception NoSuchTag where
  displayException nst =
    "No such tag: `" ++ Text.unpack (nstName nst) ++ "`"

data NotAFunction = NotAFunction
  {nafName :: Text.Text, nafActual :: Lua.Type}
  deriving (Show)

instance Exn.Exception NotAFunction where
  displayException naf =
    concat
      [ "Lua definition of `telml.",
        Text.unpack (nafName naf),
        "` not a function, found ",
        go (nafActual naf),
        " instead"
      ]
    where
      go Lua.TypeNil = "nil"
      go Lua.TypeBoolean = "boolean"
      go Lua.TypeLightUserdata = "userdata (light)"
      go Lua.TypeNumber = "number"
      go Lua.TypeString = "string"
      go Lua.TypeTable = "table"
      go Lua.TypeFunction = "function"
      go Lua.TypeUserdata = "userdata"
      go Lua.TypeThread = "thread"
      go Lua.TypeNone = "something unspeakable"

standardTags :: Text.Text -> [Text.Text] -> LuaM Text.Text
standardTags n ps =
  case (n, ps) of
    -- \em to produce italics
    ("em", [r]) -> pure ("<em>" <> r <> "</em>")
    ("em", _) -> throw (BuiltinArityMismatch 1 (length ps) n)
    -- \strong to produce bolding
    ("strong", [r]) -> pure ("<strong>" <> r <> "</strong>")
    ("strong", _) -> throw (BuiltinArityMismatch 1 (length ps) n)
    -- \li to produce list items
    ("li", [r]) -> pure ("<li>" <> r <> "</li>")
    ("li", _) -> throw (BuiltinArityMismatch 1 (length ps) n)
    _ -> throw (NoSuchTag n)

handleDoc :: TeLML.Document -> LuaM Text.Text
handleDoc = fmap mconcat . sequence . map handleFrag

handleFrag :: TeLML.Fragment -> LuaM Text.Text
handleFrag (TeLML.TextFrag text) = return text
handleFrag (TeLML.TagFrag tag) = handleTag tag

handleTag :: TeLML.Tag -> LuaM Text.Text
handleTag (TeLML.Tag n ps) = do
  ps' <- mapM handleDoc ps
  Lua.pushstring (Text.encodeUtf8 n)
  typ <- Lua.gettable 1
  case typ of
    Lua.TypeNil -> standardTags n ps'
    Lua.TypeFunction -> do
      mapM_ (Lua.pushstring . Text.encodeUtf8) ps'
      Lua.call (Lua.NumArgs (fromIntegral (length ps'))) 1
      result <- Lua.tostring 2
      case result of
        Nothing -> do
          actualtyp <- Lua.ltype 2
          error ("expected string, got" ++ show actualtyp)
        Just r -> do
          return (Text.decodeUtf8 r)
    _ -> throw (NotAFunction n typ)
