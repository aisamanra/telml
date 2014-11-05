{-# LANGUAGE OverloadedStrings #-}

module Data.TeLML.Parser (Fragment(..), parseDocument) where

import           Control.Applicative
import           Data.Attoparsec.Text
import           Data.Char (isAlphaNum)
import           Data.Text (Text)
import qualified Data.Text as T
import           Prelude hiding (takeWhile)

data Fragment
  = Chunk Text
  | Tag Text [Fragment]
    deriving (Eq, Show)

isSpecial :: Char -> Bool
isSpecial '\\' = True
isSpecial '{'  = True
isSpecial '}'  = True
isSpecial '|'  = True
isSpecial _    = False

isTagChar :: Char -> Bool
isTagChar c = isAlphaNum c

parseFg :: Parser Fragment
parseFg =  (char '\\' *> parseBk)
       <|> (char '{'  *> parseFg)
       <|> (Chunk <$> takeWhile1 (not . isSpecial))

parseBk :: Parser Fragment
parseBk =  (char '\\' *> pure (Chunk "\\"))
       <|> (char '|'  *> pure (Chunk "|"))
       <|> parseTag

parseTag :: Parser Fragment
parseTag =
  Tag <$> (takeWhile isTagChar <* many space)
      <*> (char '{' *> sepBy parseFg (char '|') <* char '}')

parseDocument :: Text -> Either String [Fragment]
parseDocument = parseOnly (many parseFg <* endOfInput)
