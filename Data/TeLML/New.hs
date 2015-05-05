import Data.Char

type Document = [Chunk]
data Chunk
  = Text String
  | Tag String [Document]
    deriving (Eq, Show)

isSpecial c = c `elem` "\\{}|"


pText :: String -> (String, Chunk)
pText = fmap Text . go
  where go ('\\':x:xs)
          | isSpecial x = fmap (x:) (go xs)
        go i@(x:xs)
          | isSpecial x = (i, "")
          | otherwise   = fmap (x:) (go xs)
        go "" = ("", "")

pTagName :: String -> (String, String)
pTagName i@(x:xs)
  | isAlpha x   = fmap (x:) (pTagName xs)
  | elem x "-_" = fmap (x:) (pTagName xs)
  | otherwise   = (i, "")

skipSpace :: String -> (String, ())
skipSpace i@(x:xs)
  | isSpace x = skipSpace xs
  | otherwise = (i, ())

pTag :: String -> (String, Chunk)
pTag i =
  let (i', name) = pTagName i
  in case skipSpace i' of
      ('{':i'') -> fmap (Tag name) (pArgs i'')
      otherwise -> error "expected start of block"

pArgs :: String -> (String, [Document])
pArgs ('|')

pChunk :: String -> (String, Chunk)
pChunk = undefined
