{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad (unless)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import qualified LamCalc.Parser as LamParser
import System.IO (hFlush, stdout)
import Text.Megaparsec (ParseErrorBundle, errorBundlePretty, parse)

prompt :: IO ()
prompt = putStr "λ> " >> hFlush stdout

parseLamExpr :: Text -> Either (ParseErrorBundle Text Void) LamParser.Expr
parseLamExpr = parse LamParser.expr "stdin"

printParseResult :: Either (ParseErrorBundle Text Void) LamParser.Expr -> IO ()
printParseResult =
  \case
    Left error -> putStrLn $ errorBundlePretty error
    Right result -> print result

main :: IO ()
main = do
  prompt
  input <- T.pack <$> getLine
  unless (input == ":q") $ (printParseResult . parseLamExpr) input >> main
