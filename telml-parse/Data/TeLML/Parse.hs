{-# LANGUAGE RecordWildCards #-}

module Data.TeLML.Parse
  ( Fragment(..)
  , Document
  , Parse
  , decode
  , parse
  , select
  , field
  , text
  , document
  , arg
  , both
  ) where

import Data.TeLML

newtype Parse t a = Parse { runParse :: t -> Either String a }

decode :: String -> Parse Document r -> Either String r
decode str content = case parse str of
  Left err -> Left err
  Right x  -> runParse content x

instance Functor (Parse t) where
  fmap f (Parse g) = Parse (\ x -> fmap f (g x))

instance Applicative (Parse t) where
  pure x = Parse (\ _ -> Right x)
  f <*> x =
    f >>= \ f' ->
    x >>= \ x' ->
    pure (f' x')

instance Monad (Parse t) where
  Parse x >>= f = Parse $ \ s ->
    case x s of
      Left err -> Left err
      Right v -> runParse (f v) s

select :: String -> Parse [Document] t -> Parse Document [t]
select name content = Parse $ \ s -> each s
  where
    each [] = return []
    each (Tag t doc:xs)
      | t == name = (:) <$> runParse content doc <*> each xs
    each (_:xs) = each xs

field :: String -> (Parse [Document] t) -> Parse Document t
field name content = Parse $ \ s -> find s
  where
    find [] = Left ("Unable to find tag \\" ++ name)
    find (Tag t doc:_)
      | t == name = runParse content doc
    find (_:xs) = find xs

arg :: Parse Document t -> Parse [Document] t
arg f = Parse $ \ s ->
  case s of
    [x] -> runParse f x
    _ -> Left ("Wrong arity for `arg`: " ++ show (length s))


both :: Parse Document a -> Parse Document b -> Parse [Document] (a, b)
both l r = Parse $ \ s ->
  case s of
    [a, b] -> (,) <$> runParse l a <*> runParse r b
    _ -> Left ("Wrong arity for `both`: " ++ show (length s))

text :: Parse Document String
text = Parse (\ s -> concat <$> traverse go s)
  where go (Text str) = Right str
        go (Tag t _)  = Left ("Expected Text fragment, found \\" ++ show t)

document :: Parse Document Document
document = Parse (\ s -> Right s)
