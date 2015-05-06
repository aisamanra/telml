{-# LANGUAGE DeriveDataTypeable #-}

module Data.TeLML.Type (Document, Fragment(..)) where

import Control.DeepSeq (NFData(..))
import Data.Data (Data)
import Data.Typeable (Typeable)
import Data.String (IsString(..))

type Document = [Fragment]
data Fragment
  = Text String
  | Tag String [Document]
    deriving (Eq, Show, Typeable, Data)

instance IsString Fragment where
  fromString = Text

instance NFData Fragment where
  rnf (Text s)  = rnf s
  rnf (Tag s l) = rnf s `seq` rnf l
