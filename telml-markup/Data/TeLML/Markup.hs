{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.TeLML.Markup where

import Control.Monad (void)
import Data.TeLML
import Text.Blaze.Html
import Text.Blaze.Html5 hiding (map, head, html)
import Text.Blaze.Html5.Attributes hiding (name, span)

import Prelude hiding (div, span)

-- | Render a TeLML document with an extra set of possible tags.
renderWith :: [(String, Renderer)] -> Document -> Either String Html
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
        go (r:rs) (t@Tag {}:ts) = go ((t:r):rs) ts
        go (r:rs) (Text s:ts)   = case splitString s of
          []  -> go (r:rs) ts
          [x] -> go ((Text x:r):rs) ts
          xs  -> go (map ((:[]) . Text) (tail xs) ++
                     ((Text (head xs):r) : rs)) ts
        go _ _ = error "[unreachable]"

-- Split a string at double-newlines.
splitString :: String -> [String]
splitString = filter (/= "") . go
  where go ('\n':'\n':xs) = "\n":go xs
        go (x:xs)         = let r:rs = go xs in ((x:r):rs)
        go ""             = [""]

-- This is just to make type signatures shorter
type HtmlE = Either String Html

type Renderer = (Fragment -> HtmlE, [Document]) -> HtmlE

-- The built-in set of tags (subject to change)
basicTags :: [(String, Renderer)]
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
  , ("br", \_ -> return br)
  , ("comment", \_ -> return "")
  , ("link"
    , \case (f,[[Text l],r]) -> let go h = a ! href (stringValue l) $ h
                                in fmap (go . sequence_) (mapM f r)
            (_,[_,_])        -> Left "link target should be string"
            _                -> Left "wrong arity for link/1"
    )
  , ("img"
    , \case (_, [[Text l]]) -> return (img ! src (stringValue l))
            (_,[_])         -> Left "image target should be string"
            _               -> Left "wrong arity for img/1"
    )
  ]
  where simpleTag :: String -> (Html -> Html) -> (String, Renderer)
        simpleTag name tag =
          ( name
          , \case (f,[rs]) -> fmap (tag . sequence_) (mapM f rs)
                  _        -> Left ("wrong arity for " ++ name ++ "/1")
          )
        listTag name tag =
          ( name
          , \case (f,rs) -> fmap (tag . sequence_) (mapM f (concat rs))
          )

-- render a single paragraph
renderPara :: [(String, Renderer)] -> Document -> Either String Html
renderPara taglist ds = fmap (p . sequence_) (mapM go ds)
  where go (Text ts) = Right (toMarkup ts)
        go (Tag tx rs) = exec tx rs taglist
        exec name args ((tag, func):tags)
          | name == tag = case func (go, args) of
            Right html -> Right html
            Left {}    -> exec name args tags
        exec name args (_:tags) = exec name args tags
        exec name args [] = Left $
          "Error: no match for tag " ++ name ++ "/" ++ show (length args)
