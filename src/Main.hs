{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Aeson (Value(..), encode, object, (.=))
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Maybe (fromJust)
import           Data.TeLML
import           Data.Vector (fromList)
import qualified Data.Text as T
import qualified Data.Text.IO as T

telmlToValue :: Fragment -> Value
telmlToValue (Chunk t)  = String t
telmlToValue (Tag t ts) = object
  [ "name"     .= String t
  , "contents" .= Array (fromList (map telmlToValue ts))
  ]

main = do
  contents <- T.getContents
  BS.putStrLn . encode . map telmlToValue . fromJust . parse $ contents
