{-# LANGUAGE OverloadedStrings #-}

module LamCalc.Untyped.Parser
  ( expr
  , Source(..)
  ) where

import           Data.Functor                  (($>))
import           Data.List                     (foldl1')
import           Data.List.NonEmpty            (nonEmpty)
import qualified Data.Set                      as Set
import           Data.String                   (IsString (..))
import           Data.Void
import           LamCalc.Untyped.Parser.Expr
import           LamCalc.Untyped.Parser.Source
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L

type Parser s = Parsec Void s

expr :: Source s => Parser s (Expr s)
expr = sp *> expr' <* eof

expr' :: Source s => Parser s (Expr s)
expr' = letBinding <|> app <|> term

app :: Source s => Parser s (Expr s)
app = label "application" $ try $ pack <$> ((:) <$> term <*> some term)
  where
    pack :: [Expr a] -> Expr a
    pack = foldl1' App

term :: Source s => Parser s (Expr s)
term = lam <|> var <|> try (lx (char '(' *> expr' <* char ')'))

lam :: Source s => Parser s (Expr s)
lam = label "lambda abstraction" $ try $ lx $ Lam <$> head <*> expr'
  where
    head = lx (char 'λ' <|> char '\\') *> some varName <* lx (char '.')

var :: Source s => Parser s (Expr s)
var = Var <$> varName

varName :: Source s => Parser s s
varName = label "variable name" $ try $ lx $ head <> body <> tail >>= check
  where
    head = fromChar <$> (letterChar <|> char '_')
    body = fromString <$> many (alphaNumChar <|> char '_')
    tail = fromString <$> many (char '\'')
    check x =
      if x `notElem` keywords
        then return x
        else unexpectedLabel ("keyword \"" <> toString x <> "\"")
    keywords = ["let", "in"]

letBinding :: Source s => Parser s (Expr s)
letBinding =
  label "let-binding" $
  try $
  Let <$> (lx (string "let") *> varName) <*> (lx (char '=') *> expr') <*>
  (lx (string "in") *> expr')

lx :: Source s => Parser s a -> Parser s a
lx = L.lexeme sp

sp :: Source s => Parser s ()
sp = L.space (oneOf [' ', '\t', '\n'] $> ()) empty empty

unexpectedLabel :: Source s => String -> Parser s a
unexpectedLabel s = failure (Label <$> nonEmpty s) Set.empty
