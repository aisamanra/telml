{-# LANGUAGE LambdaCase #-}

module Data.TeLML.Markup where

import Control.Monad (void)
import Data.TeLML
import Text.Blaze.Html
import Text.Blaze.Html5 hiding (map, head, html)
import Text.Blaze.Html5.Attributes hiding (name)

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
  [ ("em"
    , \case (f,[rs]) -> fmap (em . sequence_) (mapM f rs)
            _        -> Left "wrong arity for em/1"
    )
  , ("strong"
    , \case (f,[rs]) -> fmap (strong . sequence_) (mapM f rs)
            _        -> Left "wrong arity for strong/1"
    )
  , ("code"
    , \case (f,[rs]) -> fmap (code . sequence_) (mapM f rs)
            _        -> Left "wrong arity for code/1"
    )
  , ("link"
    , \case (f,[[Text l],r]) -> let go h = a ! href (stringValue l) $ h
                                in fmap (go . sequence_) (mapM f r)
            (_,[_,_])        -> Left "link target should be string"
            _                -> Left "wrong arity for link/1"
    )
  ]

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
