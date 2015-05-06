{-# LANGUAGE LambdaCase #-}

module Data.TeLML.Parser (Fragment(..), Document, parse) where

import Data.Char (isAlpha, isSpace)
import Data.TeLML.Type

type Result a = Either String (String, a)
type Parse a = String -> Result a

isSpecial :: Char -> Bool
isSpecial c = c `elem` "\\{}|"

throw :: a -> Either a b
throw = Left

over :: (a -> b) -> Result a -> Result b
over _ (Left err)     = Left err
over f (Right (s, x)) = Right (s, f x)

bind :: Result a -> ((String, a) -> Result b) -> Result b
bind (Left err) _ = Left err
bind (Right a)  f = f a

pText :: Parse Fragment
pText = over Text . go
  where go ('\\':x:xs)
          | isSpecial x = (x:) `over` go xs
        go i@(x:xs)
          | isSpecial x = return (i, "")
          | otherwise   = (x:) `over` go xs
        go "" = return ("", "")

pTagName :: Parse String
pTagName i@(x:xs)
  | isAlpha x   = (x:) `over` pTagName xs
  | elem x "-_" = (x:) `over` pTagName xs
  | otherwise   = return (i, "")

skipSpace :: Parse ()
skipSpace i@(x:xs)
  | isSpace x = skipSpace xs
  | otherwise = return (i, ())

pTag :: Parse Fragment
pTag i =
  bind (pTagName i) $ \ (i', name) ->
    bind (skipSpace i') $ \case
      ('{':i'', ()) -> Tag name `over` pArgs i''
      _             -> throw "expected start of block"

pArgs :: Parse [Document]
pArgs ('}':xs) = return (xs, [])
pArgs s = bind (pFragments s) $ \case
  ('|':xs, cs) -> (cs:) `over` pArgs xs
  ('}':xs, cs) -> return (xs, [cs])
  _            -> throw "[unreachable]"

pFragment :: Parse Fragment
pFragment s@('\\':c:_)
  | isSpecial c     = pText s
pFragment ('\\':xs) = pTag xs
pFragment s         = pText s

pFragments :: Parse Document
pFragments "" = return ("", [])
pFragments s@(x:_)
  | x `elem` "}|" = return (s, [])
  | otherwise     =
      bind (pFragment s) $ \case
       (s', c) -> (c:) `over` pFragments s'

parse :: String -> Either String Document
parse s = case pFragments s of
  Right ("", r) -> return r
  Right (s, _)  -> throw ("expected end of document but found " ++ show s)
  Left err      -> throw err
