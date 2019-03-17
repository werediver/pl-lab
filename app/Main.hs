{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad (unless)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import LamCalc.Untyped.Parser
import LamCalc.Untyped.Parser.Expr
import System.IO (hFlush, stdout)
import Text.Megaparsec (ParseErrorBundle, errorBundlePretty, parse)

prompt :: IO ()
prompt = putStr "λ> " >> hFlush stdout

parseLamExpr :: Text -> Either (ParseErrorBundle Text Void) Expr
parseLamExpr = parse expr "stdin"

printParseResult :: Either (ParseErrorBundle Text Void) Expr -> IO ()
printParseResult =
  \case
    Left error -> putStrLn $ errorBundlePretty error
    Right result -> print result

main :: IO ()
main = do
  prompt
  input <- T.pack <$> getLine
  unless (input == ":q") $ (printParseResult . parseLamExpr) input >> main
