module Data.TeLML(parse, Document, Fragment(..)) where

import Data.TeLML.Parser
import Data.Text (Text)

type Document = [Fragment]

parse :: Text -> Maybe Document
parse t =
  case parseDocument t of
    Left _  -> Nothing
    Right d -> Just d
