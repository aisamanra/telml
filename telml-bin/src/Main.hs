{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

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
    then throw (RedefinedTable telml)
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
        ppType (nafActual naf),
        " instead"
      ]

data NotAString = NotAString
  {nasName :: Text.Text, nasActual :: Lua.Type}
  deriving (Show)

instance Exn.Exception NotAString where
  displayException nas =
    concat
      [ "Result of calling `telml.",
        Text.unpack (nasName nas),
        "` not a string, found ",
        ppType (nasActual nas),
        " instead"
      ]

data RedefinedTable = RedefinedTable
  {rtType :: Lua.Type}
  deriving (Show)

instance Exn.Exception RedefinedTable where
  displayException rt =
    concat
      [ "Configuration file redefined `telml` to non-table; found ",
        ppType (rtType rt),
        " instead"
      ]

ppType :: Lua.Type -> String
ppType Lua.TypeNil = "nil"
ppType Lua.TypeBoolean = "boolean"
ppType Lua.TypeLightUserdata = "userdata (light)"
ppType Lua.TypeNumber = "number"
ppType Lua.TypeString = "string"
ppType Lua.TypeTable = "table"
ppType Lua.TypeFunction = "function"
ppType Lua.TypeUserdata = "userdata"
ppType Lua.TypeThread = "thread"
ppType Lua.TypeNone = "something unspeakable"

standardTags :: Text.Text -> [Text.Text] -> LuaM Text.Text
standardTags n ps =
  case n of
    -- \em to produce italics
    "em" -> simpleTag n ps (\r -> "<em>" <> r <> "</em>")
    "strong" -> simpleTag n ps (\r -> "<strong>" <> r <> "</strong>")
    "h1" -> simpleTag n ps (\r -> "<h1>" <> r <> "</h1>")
    "h2" -> simpleTag n ps (\r -> "<h2>" <> r <> "</h2>")
    "p" -> simpleTag n ps (\r -> "<p class=\"para\">" <> r <> "</p>")
    "blockquote" -> simpleTag n ps (\r -> "<blockquote>" <> r <> "</blockquote>")
    "tt" -> simpleTag n ps (\r -> "<code>" <> r <> "</code>")
    "code" -> simpleTag n ps (\r -> "<pre><code>" <> r <> "</code></pre>")
    "center" -> simpleTag n ps (\r -> "<div class=\"center\">" <> r <> "</div>")
    -- some of the variadic ones
    "ul" ->
      pure ("<ul>" <> mconcat ["<li>" <> p <> "</li>" | p <- ps] <> "</ul>")
    "ol" ->
      pure ("<ol>" <> mconcat ["<li>" <> p <> "</li>" | p <- ps] <> "</ol>")
    "br" ->
      pure "<br/>"
    "comment" -> pure ""
    "link" -> case ps of
      [address, text] ->
        pure ("<a href=\"" <> address <> "\">" <> text <> "</a>")
      _ -> throw (BuiltinArityMismatch 2 (length ps) n)
    "img" -> case ps of
      [address] ->
        pure ("<img src=\"" <> address <> "\">")
      [address, altText] ->
        pure ("<img src=\"" <> address <> "\" alt=\"" <> altText <> "\">")
      _ -> throw (BuiltinArityMismatch 1 (length ps) n)
    _ -> throw (NoSuchTag n)

simpleTag :: Text.Text -> [Text.Text] -> (Text.Text -> Text.Text) -> LuaM Text.Text
simpleTag _ [item] result = pure (result item)
simpleTag name items _ = throw (BuiltinArityMismatch 1 (length items) name)

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
          throw (NotAString n actualtyp)
        Just r -> do
          return (Text.decodeUtf8 r)
    _ -> throw (NotAFunction n typ)
