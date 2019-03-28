{-# LANGUAGE OverloadedStrings #-}

module LamCalc.Untyped.Parser
  ( expr
  ) where

import           Data.Functor                (($>))
import           Data.List                   (foldl1')
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Data.Void
import           LamCalc.Untyped.Parser.Expr
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer  as L

type Parser = Parsec Void Text

expr :: Parser Expr
expr = sp *> expr' <* eof

expr' :: Parser Expr
expr' = app <|> term

app :: Parser Expr
app = label "application" $ try $ pack <$> ((:) <$> term <*> some term)
  where
    pack :: [Expr] -> Expr
    pack = foldl1' App

term :: Parser Expr
term = lam <|> var <|> try (lx (char '(' *> expr' <* char ')'))

lam :: Parser Expr
lam = label "lambda abstraction" $ try $ lx $ Lam <$> head <*> expr'
  where
    head = lx (char 'λ' <|> char '\\') *> some varName <* lx (char '.')

var :: Parser Expr
var = Var <$> varName

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
