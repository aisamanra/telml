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
import qualified System.Exit as Sys

-- | The main driver
main :: IO ()
main = do
  -- parse command-line options
  options <- parseOpts
  -- read the source file
  telmlSource <- case optInputFile options of
    Nothing -> getContents
    Just f -> readFile f
  -- attempt to parse it
  telml <- case TeLML.parse telmlSource of
    Right str -> return str
    Left err -> do
      putStrLn err
      Sys.exitFailure
  -- read the Lua source file, if provided
  luaSource <- case optTagFile options of
    Nothing -> return ""
    Just f -> BS.readFile f
  -- run everything needed in the Lua context (i.e. evaluating the
  -- source and then using it to interpret tags)
  result <- Lua.runEither (luaMain options luaSource telml)
  -- either print the result or print the error nicely
  case result of
    Right msg -> Text.putStr msg
    Left err -> do
      putStrLn (Exn.displayException err)
      Sys.exitFailure

-- * Lua stuff

-- | Everything in Lua should be in this monad, which includes our
-- custom error type
type LuaM r = Lua.LuaE Error r

-- | Evaluate the provided Lua source code and then use it to
-- interpret the `TeLML.Document`.
luaMain :: Options -> BS.ByteString -> TeLML.Document -> LuaM Text.Text
luaMain opts luaSource doc = do
  -- load the basic libraries so we have access to stuff like `ipairs`
  Lua.openbase
  Lua.pop 1

  -- create the global `telml` table as a namespace for tags
  Lua.newtable
  Lua.setglobal "telml"

  -- evaluate the source file. (We don't care what it evaluates to.)
  _ <- Lua.dostring luaSource

  -- make sure that the user didn't do something funky like redefine
  -- the global `telml` to a string.
  telml <- Lua.getglobal "telml"
  if telml /= Lua.TypeTable
    then throw (RedefinedTable telml)
    else return ()

  -- walk over the document, evaluating as we go
  handleDoc opts doc

-- | Convert a `TeLML.Document` into a piece of `Text`
handleDoc :: Options -> TeLML.Document -> LuaM Text.Text
handleDoc opts = fmap mconcat . sequence . map (handleFrag opts)

-- | Convert a `TeLML.Fragment` into a piece of `Text` with the
-- relevant tag evaluation
handleFrag :: Options -> TeLML.Fragment -> LuaM Text.Text
handleFrag _ (TeLML.TextFrag text) = return text
handleFrag opts (TeLML.TagFrag tag) = handleTag opts tag

-- | Evaluate a tag in light of both the Lua source and the provided
-- options
handleTag :: Options -> TeLML.Tag -> LuaM Text.Text
handleTag opts (TeLML.Tag n ps) = do
  -- evaluate the "arguments" first
  ps' <- mapM (handleDoc opts) ps
  -- look up the tag in the table
  Lua.pushstring (Text.encodeUtf8 n)
  -- check the type of the thing we've gotten out. (If it wasn't
  -- present in the table, we'll get `nil`.)
  typ <- Lua.gettable 1
  case typ of
    Lua.TypeNil
      -- Defer to the standard tags by default
      | optUseDefaultTags opts -> standardTags n ps'
      -- ...but if the user opted out, then throw errors
      | otherwise -> throw (NoSuchTag n)
    -- if it's a function, then we can call it!
    Lua.TypeFunction -> do
      -- it's already on the stack, so now we need to add all the
      -- arguments to the stack. They're all strings, so push the
      -- appropriate bytestrings there
      mapM_ (Lua.pushstring . Text.encodeUtf8) ps'
      -- Call the function with the number of args we've passed, and
      -- expect a single return value
      Lua.call (Lua.NumArgs (fromIntegral (length ps'))) 1
      -- look at the top thing on the stack to make sure it's a string
      -- (or convertible)
      result <- Lua.tostring 2
      case result of
        -- if we got `Nothing`, then it's not a string; throw an error
        Nothing -> do
          actualtyp <- Lua.ltype 2
          throw (NotAString n actualtyp)
        -- otherwise, it's a string, so pass it back down!
        Just r -> do
          Lua.pop 1
          return (Text.decodeUtf8 r)
    -- if it's not `nil` _or_ a function, then produce an error about
    -- it
    _ -> throw (NotAFunction n typ)

-- * Errors and error-handling

-- | We wrap the usual LuaHS error type in our own
data Error
  = LuaError Lua.Exception
  | TeLMLError Exn.SomeException
  deriving (Show)

instance Exn.Exception Error where
  -- did you know this tries to use `show` by default? I really wish
  -- Haskell had something like the str/repr distinction or
  -- debug/display distinction. Trying to force every string
  -- representation into `show` is nonsense.
  displayException (LuaError err) = Exn.displayException err
  displayException (TeLMLError err) = Exn.displayException err

-- We need this in order to use our custom error type as a wrapper
-- around Lua ones
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

-- | A function that makes it easy to throw our own exceptions
throw :: Exn.Exception e => e -> LuaM a
throw =
  Lua.liftIO . Exn.throwIO . TeLMLError . Exn.toException

-- ** Custom errors

-- | This represents when a builtin tag expects a specific arity and
-- we invoke it with the wrong one
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

-- | This is thrown when we can't find any tag with the relevant name
data NoSuchTag = NoSuchTag {nstName :: Text.Text} deriving (Show)

instance Exn.Exception NoSuchTag where
  displayException nst =
    "No such tag: `" ++ Text.unpack (nstName nst) ++ "`"

-- | This is thrown when the Lua defined something with this name, but
-- it wasn't actually a function we could call
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

-- | This is thrown when we call a Lua function and it returns
-- something which either isn't a string or isn't trivially
-- convertable to a string (like a number).
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

-- | This is thrown if the code for some reason tries to redefine
-- `telml` into something that's not a table.
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

-- | Print a Lua type nicely (for error message purposes)
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

-- * Tag definitions

-- | Try to interpret this tag from the standard set, throwing an
-- error if it doesn't exist
standardTags :: Text.Text -> [Text.Text] -> LuaM Text.Text
standardTags n ps =
  case n of
    -- \em to produce italics
    "em" -> simpleTag n ps (\r -> "<em>" <> r <> "</em>")
    "strong" -> simpleTag n ps (\r -> "<strong>" <> r <> "</strong>")
    "h1" -> simpleTag n ps (\r -> "<h1>" <> r <> "</h1>")
    "h2" -> simpleTag n ps (\r -> "<h2>" <> r <> "</h2>")
    "h3" -> simpleTag n ps (\r -> "<h3>" <> r <> "</h3>")
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

-- * Options and option-parsing

data Options = Options
  { optInputFile :: Maybe String,
    optUseDefaultTags :: Bool,
    optTagFile :: Maybe String
  }
  deriving (Eq, Show)

optionDescriptions :: [Opt.OptDescr (Options -> Options)]
optionDescriptions =
  [ Opt.Option
      ['n']
      ["no-default-tags"]
      (Opt.NoArg (\o -> o {optUseDefaultTags = False}))
      "Do not include any default tags",
    Opt.Option
      ['t']
      ["tags"]
      (Opt.ReqArg (\f o -> o {optTagFile = Just f}) "[tagfile.lua]")
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
  case Opt.getOpt Opt.Permute optionDescriptions args of
    (flags, [], []) ->
      return (foldl (flip id) def flags)
    (flags, [input], []) ->
      return (foldl (flip id) def flags) {optInputFile = Just input}
    (_, _, errors) -> do
      putStr (unlines errors)
      putStrLn (Opt.usageInfo "USAGE: telml [input.telml] [-n] [-t tagfile.lua]" optionDescriptions)
      Sys.exitFailure
