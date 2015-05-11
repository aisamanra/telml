{-# LANGUAGE DeriveDataTypeable #-}

module Data.TeLML.Type (Document, Fragment(..)) where

import Control.DeepSeq (NFData(..))
import Data.Data (Data)
import Data.Typeable (Typeable)
import Data.String (IsString(..))

-- | A 'Document' is zero or more 'Fragment's.
type Document = [Fragment]

-- | A 'Fragment' is either a snippet of text (as indicated by the
--   'Text' constructor) or a tag (as indicated by the 'Tag'
--   constructor). The former is a raw string, and the latter consists
--   of a name followed by zero or more 'Document's.
data Fragment
  = Text String
  | Tag String [Document]
    deriving (Eq, Show, Typeable, Data)

instance IsString Fragment where
  fromString = Text

instance NFData Fragment where
  rnf (Text s)  = rnf s
  rnf (Tag s l) = rnf s `seq` rnf l
