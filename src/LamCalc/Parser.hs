{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module LamCalc.Parser
  ( Expr
  , expr
  ) where

import Data.Functor (($>), (<&>))
import Data.Functor.Classes (Show1)
import Data.Functor.Foldable
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Debug
import Text.Show.Deriving

type VarName = Text

data ExprF a
  = VarF VarName
  | LamF VarName
         a
  | AppF a
         a

$(deriveShow1 ''ExprF)

type Expr = Fix ExprF

type Parser = Parsec Void Text

expr :: Parser Expr
expr = sp *> expr' <* eof

expr' :: Parser Expr
expr' = app <|> term

app :: Parser Expr
app = label "application" $ try $ pack <$> ((:) <$> term <*> some term)
  where
    pack :: [Expr] -> Expr
    pack = foldl1 (\f x -> Fix (AppF f x))

term :: Parser Expr
term = lam <|> var <|> (char '(' *> expr' <* char ')')

lam :: Parser Expr
lam = label "lambda abstraction" $ try $ lx $ pack <$> head <*> term
  where
    pack paramName body = Fix (LamF paramName body)
    head = lx (char 'λ' <|> char '\\') *> varName <* lx (char '.')

var :: Parser Expr
var = Fix . VarF <$> varName

varName :: Parser VarName
varName = label "variable name" $ try $ lx $ head <> body <> tail
  where
    head = T.singleton <$> (letterChar <|> char '_')
    body = T.pack <$> many (alphaNumChar <|> char '_')
    tail = T.pack <$> many (char '\'')

lx :: Parser a -> Parser a
lx = L.lexeme sp

sp :: Parser ()
sp = L.space (oneOf [' ', '\t', '\n'] $> ()) empty empty
