{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.TeLML.Markup where

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

-- This is just to make type signatures shorter
type HtmlE = Either String Html

type Renderer = (Fragment -> HtmlE, [Document]) -> HtmlE

infixr 5 :=>
data a :=> b = a :=> b deriving (Eq, Show)

class TagArguments t where
  toType :: t -> [T.Text]
  taExec :: t -> [Document] -> (Fragment -> HtmlE) -> Maybe HtmlE

instance (s ~ String, h ~ Html, m ~ Either s h) => TagArguments (() -> m) where
  toType _ = []
  taExec r [] go = Just (r ())
  taExec r _  _  = Nothing

instance TagArguments r => TagArguments (Str -> r) where
  toType _ = "_" : toType (undefined :: r)
  taExec f ([TextFrag t]:rs) go = taExec (f (Str t)) rs go
  taExec _ _                 _  = Nothing

instance TagArguments r => TagArguments (Doc -> r) where
  toType _ = "string" : toType (undefined :: r)
  taExec f (doc:rs) go = taExec (f (Doc doc)) rs go
  taExec _ []       _  = Nothing

instance TagArguments r => TagArguments (H -> r) where
  toType _ = "_" : toType (undefined :: r)
  taExec f (doc:rs) go =
    let h = fmap sequence_ (mapM go doc)
    in case h of
      Left err -> return (Left err)
      Right h' -> taExec (f (H h')) rs go
  taExec _ []       _  = Nothing

instance (s ~ String, h ~ Html, m ~ Either s h) => TagArguments (Hs -> m) where
  toType _ = ["..."]
  taExec f docs go =
    let h = fmap sequence_ (mapM go (concat docs))
    in case h of
      Left err -> return (Left err)
      Right h' -> return (f (Hs h'))
  taExec _ []       _  = Nothing

data Tag' = forall t. TagArguments t => Tag'
  { tgName :: T.Text
  , tgFunc :: t
  }

newtype Str = Str T.Text deriving (Eq, Show)
newtype Doc = Doc Document deriving (Eq, Show)
newtype H = H Html
newtype Hs = Hs Html
newtype Docs = Docs [Document] deriving (Eq, Show)

exec :: T.Text -> [Document] -> [Tag'] -> Either String Html
exec name args (Tag' tag func:_)
  | name == tag = case taExec func args undefined of
      Nothing -> Left $ unwords [ "Tag"
                                , T.unpack ('\\' `T.cons` name)
                                , "expects argument structure"
                                , T.unpack ('\\' `T.cons` name `T.append`
                                            T.intercalate "|" (toType func))
                                ]
      Just x -> x
exec name args (_:rs) = exec name args rs
exec name args [] = Left $
  "Error: no match for tag " ++ T.unpack name ++ "/" ++ show (length args)

data Args t where
  EndArg      ::              Args ()
  ListArg     ::              Args [Document]
  FragmentArg :: Args rest -> Args (Document :=> rest)
  TextArg     :: Args rest -> Args (T.Text :=> rest)

matchArguments :: Args t -> [Document] -> Maybe t
matchArguments EndArg [] = return ()
matchArguments EndArg _  = Nothing
matchArguments ListArg ds = return ds
matchArguments (FragmentArg rs) (d:ds) =
  (d :=>) `fmap` matchArguments rs ds
matchArguments (FragmentArg EndArg) [] = return ([TextFrag ""] :=> ())
matchArguments (FragmentArg _) [] = Nothing
matchArguments (TextArg rs) ([TextFrag t]:ds) =
  (t :=>) `fmap` matchArguments rs ds
matchArguments (TextArg _) _ = Nothing

argType :: Args t -> T.Text
argType t = "{" `T.append` T.intercalate "|" (toType t) `T.append` "}"
  where
    toType :: Args t -> [T.Text]
    toType EndArg = []
    toType ListArg = ["..."]
    toType (FragmentArg rs) = "_" : toType rs
    toType (TextArg rs) = "string" : toType rs

data TagDescription = forall t. TagDescription
  { tdName   :: T.Text
  , tdArgs   :: Args t
  , tdAction :: t -> (Fragment -> HtmlE) -> HtmlE
  }

simpleTag :: T.Text -> (Markup -> Html) -> TagDescription
simpleTag name tag = TagDescription
  { tdName = name
  , tdArgs = FragmentArg EndArg
  , tdAction = \ (fragment :=> ()) f ->
    fmap (tag . sequence_) (mapM f fragment)
  }

listTag :: T.Text -> (Markup -> Html) -> TagDescription
listTag name tag = TagDescription
  { tdName = name
  , tdArgs = ListArg
  , tdAction = \ rs f ->
    fmap (tag . sequence_) (mapM f (concat rs))
  }


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
  , listTag "center" (\ rs -> div ! class_ "center" $ rs)

  , TagDescription "br" EndArg (\ () _ -> return br)
  , TagDescription "comment" ListArg (\ _ _ -> return "")
  , TagDescription "link" (TextArg (FragmentArg EndArg)) $ \ (l :=> r :=> ()) f ->
      let go h = a ! href (toValue l) $ h
      in fmap (go . sequence_) (mapM f r)
  , TagDescription "img" (TextArg (TextArg EndArg)) $ \ (l :=> r :=> ()) _ ->
      return (img ! src (toValue l) ! alt (toValue r))
  ]

-- render a single paragraph
renderPara :: [TagDescription] -> Document -> Either String Html
renderPara taglist ds = fmap (p . sequence_) (mapM go ds)
  where go (TextFrag ts) = Right (toMarkup ts)
        go (TagFrag (Tag tx rs)) = exec tx rs taglist
        exec name args (TagDescription tag rg func:_)
          | name == tag = case matchArguments rg args of
              Nothing -> Left $ unwords [ "Tag"
                                        , T.unpack ('\\' `T.cons` name)
                                        , "expects argument structure"
                                        , T.unpack ('\\' `T.cons` name `T.append` argType rg)
                                        ]
              Just x -> func x go
        exec name args (_:tags) = exec name args tags
        exec name args [] = Left $
          "Error: no match for tag " ++ T.unpack name ++ "/" ++ show (length args)

-- The built-in set of tags (subject to change)
basicTags' :: [Tag']
basicTags' =
  [ simpleTag' "em" em
  , simpleTag' "strong" strong
  , simpleTag' "li" li
  , simpleTag' "h1" h1
  , simpleTag' "h2" h2
  , simpleTag' "p" (\ rs -> span ! class_ "para" $ rs)
  , simpleTag' "blockquote" blockquote
  , simpleTag' "tt" code
  , simpleTag' "code" (pre . code)
  , simpleTag' "ttcom" (\ rs -> span ! class_ "comment" $ rs)
  , simpleTag' "ttkw"  (\ rs -> span ! class_ "keyword" $ rs)
  , simpleTag' "ttcn"  (\ rs -> span ! class_ "constr" $ rs)
  , simpleTag' "ttstr" (\ rs -> span ! class_ "string" $ rs)

  , listTag' "ul" ul
  , listTag' "ol" ol
  , listTag' "center" (\ rs -> div ! class_ "center" $ rs)

  , Tag' "br" (\ () -> return br)
  , Tag' "comment" (\ () -> return "")
  , Tag' "link" (\ (Str l) (H h) () ->
                   let go h = a ! href (toValue l) $ h
                   in return (go h))
  , Tag' "img" (\ (Str l) (Str r) () ->
                  return (img ! src (toValue l) ! alt (toValue r)))
  ]

simpleTag' :: T.Text -> (Markup -> Html) -> Tag'
simpleTag' name tag = Tag'
  { tgName = name
  , tgFunc = \ (H h) () ->
      return (tag h)
  }

listTag' :: T.Text -> (Markup -> Html) -> Tag'
listTag' name tag = Tag'
  { tgName = name
  , tgFunc = \ (Hs hs) -> return (tag hs)
  }

-- render a single paragraph
renderPara' :: [Tag'] -> Document -> Either String Html
renderPara' taglist ds = fmap (p . sequence_) (mapM go ds)
  where go (TextFrag ts) = Right (toMarkup ts)
        go (TagFrag (Tag tx rs)) = exec tx rs taglist
