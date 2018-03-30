{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Data.TeLML.Markup
( renderWith
, render
, basicTags
, mkTag
, simpleTag
, listTag
, H(..)
, Hs(..)
, Str(..)
, TagDescription
) where

import Control.Monad (void)
import Data.TeLML
import qualified Data.Text as T
import Text.Blaze.Html
import Text.Blaze.Html5 hiding (map, head, html)
import Text.Blaze.Html5.Attributes hiding (name, span)

import Prelude hiding (div, span)

-- | Render a TeLML document with an extra set of possible tags.
renderWith :: [TagDescription] -> Document -> Either String Html
renderWith rs =
  fmap (void . sequence) . mapM (renderPara (basicTags ++ rs)) . gatherPara

-- | Render a TeLML document with the default set of tags.
render :: Document -> Either String Html
render = renderWith []

-- This is a gross function, but I'm not sure how to decompose it any
-- other way. It takes a Document---i.e. a set of Fragments---and
-- splits it apart whenever it comes across double newlines.
gatherPara :: Document -> [Document]
gatherPara = reverse . map reverse . go [[]]
  where go rs [] = rs
        go (r:rs) (t@TagFrag {}:ts) = go ((t:r):rs) ts
        go (r:rs) (TextFrag s:ts)   = case splitString s of
          []  -> go (r:rs) ts
          [x] -> go ((TextFrag x:r):rs) ts
          xs  -> go (map ((:[]) . TextFrag) (tail xs) ++
                     ((TextFrag (head xs):r) : rs)) ts
        go _ _ = error "[unreachable]"

-- Split a string at double-newlines.
splitString :: T.Text -> [T.Text]
splitString = T.splitOn "\n\n"


-- | The 'TagArguments' class allow us to define a new tag with a name
-- and a simple function, and cuts out a lot of the boilerplate.
class TagArguments t where
  toType :: t -> [T.Text]
  taExec :: t
         -> [Document]
         -> (Fragment -> Either String Html)
         -> Maybe (Either String Html)

instance TagArguments Html where
  toType _ = []
  taExec h [] _ = Just (Right h)
  taExec _ _  _ = Nothing

instance TagArguments r => TagArguments (Str -> r) where
  toType _ = "str" : toType (undefined :: r)
  taExec f ([TextFrag t]:rs) go = taExec (f (Str t)) rs go
  taExec _ _                 _  = Nothing

instance TagArguments r => TagArguments (Maybe Str -> r) where
  toType _ = "str?" : toType (undefined :: r)
  taExec f ([TextFrag t]:rs) go = taExec (f (Just (Str t))) rs go
  taExec f []                go = taExec (f Nothing) [] go
  taExec _ _                 _  = Nothing

instance TagArguments r => TagArguments (H -> r) where
  toType _ = "frag" : toType (undefined :: r)
  taExec f (doc:rs) go =
    let h = fmap sequence_ (mapM go doc)
    in case h of
      Left err -> return (Left err)
      Right h' -> taExec (f (H h')) rs go
  taExec _ []       _  = Nothing

instance (h ~ Html) => TagArguments (Hs -> h) where
  toType _ = ["..."]
  taExec f docs go =
    let h = mapM (fmap sequence_ . mapM go) docs
    in case h of
      Left err -> return (Left err)
      Right hs -> return (Right (f (Hs hs)))

data TagDescription
  = forall t. TagArguments t =>
    TagDescription T.Text t

-- | The 'Str' newtype will match a literal chunk of non-formatted,
-- non-structured text.
newtype Str = Str { fromStr :: T.Text }

-- | The 'H' newtype will match a single, pre-rendered argument
newtype H = H { fromHtml :: Html }

-- | The 'Hs' newtype will match a concatenated set of pre-rendered
-- arguments
newtype Hs = Hs { fromHtmlList :: [Html] }

mkTag :: TagArguments t => T.Text -> t -> TagDescription
mkTag = TagDescription

-- The built-in set of tags (subject to change)
basicTags :: [TagDescription]
basicTags =
  [ simpleTag "em" em
  , simpleTag "strong" strong
  , simpleTag "li" li
  , simpleTag "h1" h1
  , simpleTag "h2" h2
  , simpleTag "p" (\ rs -> span ! class_ "para" $ rs)
  , simpleTag "blockquote" blockquote
  , simpleTag "tt" code
  , simpleTag "code" (pre . code)
  , simpleTag "ttcom" (\ rs -> span ! class_ "comment" $ rs)
  , simpleTag "ttkw"  (\ rs -> span ! class_ "keyword" $ rs)
  , simpleTag "ttcn"  (\ rs -> span ! class_ "constr" $ rs)
  , simpleTag "ttstr" (\ rs -> span ! class_ "string" $ rs)

  , listTag "ul" ul
  , listTag "ol" ol
  , mkTag "list" (\ (Hs hs) -> ul $ mapM_ li hs)
  , listTag "center" (\ rs -> div ! class_ "center" $ rs)

  , TagDescription "br" br
  , TagDescription "comment" ("" :: Html)
  , TagDescription "link" (\ (Str l) (H h) -> a ! href (toValue l) $ h)
  , TagDescription "img" $ \ (Str l) altText -> case altText of
      Just r  -> img ! src (toValue l) ! alt (toValue (fromStr r))
      Nothing -> img ! src (toValue l)
  ]

simpleTag :: T.Text -> (Markup -> Html) -> TagDescription
simpleTag name tag = mkTag name (tag . fromHtml)

listTag :: T.Text -> (Markup -> Html) -> TagDescription
listTag name tag = mkTag name (tag . mconcat . fromHtmlList)

argsFor :: TagArguments t => t -> T.Text
argsFor func = T.cons '{' (T.snoc (T.intercalate "|" (toType func)) '}')

-- render a single paragraph
renderPara :: [TagDescription] -> Document -> Either String Html
renderPara taglist ds = fmap (p . sequence_) (mapM go ds)
  where go (TextFrag ts) = Right (toMarkup ts)
        go (TagFrag (Tag tx rs)) = exec tx rs taglist
        exec name args (TagDescription tag func:_)
          | name == tag = case taExec func args go of
              Nothing -> Left $ unwords
                [ "Tag"
                , T.unpack ('\\' `T.cons` name)
                , "expects argument structure"
                , T.unpack ('\\' `T.cons` name `T.append` argsFor func)
                ]
              Just x -> x
        exec name args (_:rs) = exec name args rs
        exec name args [] = Left $
          "Error: no match for tag " ++ T.unpack name ++ "/" ++ show (length args)
