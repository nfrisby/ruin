module Data.Ruin.QQ.Parser (
  QQ(..),
  pQQ,
  ) where

import           Data.Char (isSpace)
import           Data.Functor (void)
import           Data.Maybe (fromMaybe)

import           Text.Parsec
import           Text.Parsec.String (Parser)

data QQ = MkQQ
  (Maybe String)
  [(Bool,String,String)]
  deriving Show

pQQ :: Parser QQ
pQQ = do
  optional gap
  typename <- optionMaybe (pTypename <* gap)
  affixes <- optionMaybe (pAffixes <* gap)
  case affixes of
    Just (Nothing,Nothing) -> fail "Refusing a degenerate affix specification: remove (...)."
    _ -> return ()
  binders <- go
  eof
  pure (MkQQ typename (map (interpretAffixes affixes) binders))

  where

  go = ((:) <$> pBinder <*> go1) <|> pure []
  go1 = (gap *> go) <|> pure []

interpretAffixes ::
     Maybe (Maybe String,Maybe String)
  -> (b,Maybe String,String) -> (b,String,String)
interpretAffixes x (b,mvar,field) = (b,var,field)
  where
  var = case fromMaybe field mvar of
    "_" -> "_"   -- do not apply affixes to _
    o -> maybe id affix x o

  affix (pre,suf) = maybe id (++) pre . maybe id (flip (++)) suf

gap :: Parser ()
gap = void $ many1 $ satisfy isSpace

rest :: Parser Char
rest = char '_' <|> char '\'' <|> alphaNum

pTypename :: Parser String
pTypename = (:) <$> upper <*> many rest <?> "type name"

pAffixes :: Parser (Maybe String,Maybe String)
pAffixes =
    (<?> "affix spec")
  $ between (char '(') (char ')')
  $ (,) <$> optionMaybe pPrefix <* ellipsis <*> optionMaybe pSuffix

ellipsis :: Parser ()
ellipsis = () <$ string "..."

pPrefix :: Parser String
pPrefix = pVar

pSuffix :: Parser String
pSuffix = many1 $ char '_' <|> char '\'' <|> alphaNum

pBinder :: Parser (Bool,Maybe String,String)
pBinder = try (char '!' *> pAt True) <|> pAt False <?> "binder"

pAt :: Bool -> Parser (Bool,Maybe String,String)
pAt b = (,,) b <$> optionMaybe (try (pVar <* char '@')) <*> pField

pVar :: Parser String
pVar = (:) <$> (char '_' <|> lower) <*> many rest <?> "Haskell variable name"

pField :: Parser String
pField = (many1 $ satisfy $ not . isSpace) <?> "field name"
