{-# LANGUAGE DeriveDataTypeable #-}

module Data.TeLML.Type (Document, Fragment(..), Tag(..)) where

import           Control.DeepSeq (NFData(..))
import           Data.Data (Data)
import qualified Data.Text as T
import           Data.Typeable (Typeable)
import           Data.String (IsString(..))

-- | A 'Document' is zero or more 'Fragment's.
type Document = [Fragment]

-- | A 'Fragment' is either a snippet of text (as indicated by the
--   'Text' constructor) or a tag (as indicated by the 'Tag'
--   constructor). The former is a raw string, and the latter consists
--   of a name followed by zero or more 'Document's.
data Fragment
  = TextFrag T.Text
  | TagFrag Tag
    deriving (Eq, Show, Typeable, Data)

data Tag = Tag
  { tagName    :: T.Text
  , tagPayload :: [Document]
  } deriving (Eq, Show, Typeable, Data)

instance IsString Fragment where
  fromString = TextFrag . fromString

instance NFData Fragment where
  rnf (TextFrag s)  = rnf s
  rnf (TagFrag t) = rnf t

instance NFData Tag where
  rnf (Tag n l) = rnf n `seq` rnf l
