{-# LANGUAGE LambdaCase #-}

module Data.TeLML.Parser (Fragment(..), Document, parse) where

import Data.Char (isAlpha, isAlphaNum, isSpace)
import Data.TeLML.Type

type Result a = Either String (String, a)
type Parse a = String -> Result a

-- All of these characters are the ones which need escaping if used
-- inside a document.
isSpecial :: Char -> Bool
isSpecial c = c `elem` "\\{}|"

-- This is here for, uh, aesthetic reasons.
throw :: a -> Either a b
throw = Left

-- This is 'fmap' named in such a way that it does not conflict with
-- 'fmap'.
over :: (a -> b) -> Result a -> Result b
over _ (Left err)     = Left err
over f (Right (s, x)) = Right (s, f x)

{- And this is a monadic bind. You'll note that this basically has the
 - same type as almost any other monadic parser combinator library, so
 - you might be wondering why I'm not just using parsec. On the other
 - hand, this is literally the only function I actually need there, so
 - why have another dependency I need to keep up-to-date when I could
 - just define this trivial function here?
 -
 - I'm also not defining this for `Monad` itself because then I'd have
 - to deal with newtype wrappers and defining methods for the whole
 - Functor=>Applicative=>Monad hierarchy, when all I really need is
 - this.
-}
bind :: Result a -> ((String, a) -> Result b) -> Result b
bind (Left err) _ = Left err
bind (Right a)  f = f a

-- Parse a text fragment, handling escapes. This will end as soon as it
-- sees any non-escaped special character.
pText :: Parse Fragment
pText = over Text . go
  where go ('\\':x:xs)
          | isSpecial x = (x:) `over` go xs
        go i@(x:xs)
          | isSpecial x = return (i, "")
          | otherwise   = (x:) `over` go xs
        go "" = return ("", "")

-- Parse a tag name of length >= 0.
pTagName :: Parse String
pTagName s = go s `bind` ensureName
  where go i@(x:xs)
          | isAlphaNum x = (x:) `over` go xs
          | elem x "-_"  = (x:) `over` go xs
          | otherwise    = return (i, "")
        go [] = throw "unexpected end-of-document while parsing tag"
        ensureName (xs, name)
          | length name == 0 =
              throw "expected tag name after `\\'"
          | not (isAlpha (head name)) =
              throw "tag names must begin with an alphabetic character"
          | otherwise = return (xs, name)

-- Skip any space charaters, returning () for the first non-space
-- character (including EOF).
skipSpace :: Parse ()
skipSpace i@(x:xs)
  | isSpace x = skipSpace xs
  | otherwise = return (i, ())
skipSpace _ = return ("", ())

-- Parse a tag assuming that a backslash has already been encountered.
pTag :: Parse Fragment
pTag i =
  bind (pTagName i) $ \ (i', name) ->
    bind (skipSpace i') $ \case
      ('{':i'', ()) -> Tag name `over` pArgs i''
      ("",_)        -> throw "unexpected end-of-document while parsing tag"
      _             -> throw "expected start of block"

-- Parse the vertical-bar-separated arguments to a tag, ending when a
-- right curly brace is encountered.
pArgs :: Parse [Document]
pArgs ('}':xs) = return (xs, [])
pArgs s = bind (pFragments s) $ \case
  ('|':xs, cs) -> (cs:) `over` pArgs xs
  ('}':xs, cs) -> return (xs, [cs])
  _            -> throw "[unreachable]"

-- Parse any fragment, deciding whether to parse it as a tag or a text chunk
pFragment :: Parse Fragment
pFragment s@('\\':c:_)
  | isSpecial c     = pText s
pFragment ('\\':xs) = pTag xs
pFragment s         = pText s

-- Parse multiple fragments, ending when it encounters a }, or |, or end-of-file.
pFragments :: Parse Document
pFragments "" = return ("", [])
pFragments ('{':s) = bind (pFragments s) $ \case
  ('}':xs, cs) -> bind (pFragments xs) $ \(xs', cs') -> return (xs', cs ++ cs')
  (x:_,    _)  -> throw ("unexpected " ++ show x ++ "; expected '}'")
  ([],     _)  -> throw ("unexpected end-of-document while parsing block")
pFragments s@(x:_)
  | x `elem` "}|" = return (s, [])
  | otherwise     =
      bind (pFragment s) $ \case
       (s', c) -> (c:) `over` pFragments s'

-- | Parse a string into a @TeLML@ 'Fragment'.
parse :: String -> Either String Document
parse str = case pFragments str of
  Right ("", r)     -> return r
  Right ('}':_, _)  -> throw ("Found unmatched '}' in document")
  Right (s, _)      -> throw ("expected end of document but found " ++ show s)
  Left err          -> throw err
