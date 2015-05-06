{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Aeson (Value(..), encode, object, (.=))
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.TeLML
import           Data.Vector (fromList)
import qualified Data.Text as T

telmlToValue :: Fragment -> Value
telmlToValue (Text t)  = String (T.pack t)
telmlToValue (Tag t ts) = object
  [ "name"     .= String (T.pack t)
  , "contents" .= arr (map (arr . map telmlToValue) ts)
  ]
  where arr = Array . fromList

fromRight (Right x) = x
fromRight _ = undefined

main = do
  r <- fmap parse getContents
  case r of
    Right x  -> BS.putStrLn . encode . map telmlToValue $ x
    Left err -> putStrLn err
