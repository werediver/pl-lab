{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad               (unless)
import qualified Data.Text                   as T
import           Data.Text.Prettyprint.Doc   (Pretty, pretty)
import           Data.Void
import           LamCalc.Untyped.Naive       (desugar, nf)
import           LamCalc.Untyped.Parser
import qualified LamCalc.Untyped.Parser.Expr as P
import           System.IO                   (hFlush, stdout)
import           Text.Megaparsec             (ParseErrorBundle, errorBundlePretty, parse)

prompt :: IO ()
prompt = putStr "λ> " >> hFlush stdout

parseLamExpr :: Source s => s -> Either (ParseErrorBundle s Void) (P.Expr s)
parseLamExpr = parse expr "stdin"

printResult :: (Source s, Pretty a) => Either (ParseErrorBundle s Void) a -> IO ()
printResult =
  \case
    Left error -> putStrLn $ errorBundlePretty error
    Right result -> print $ pretty result

main :: IO ()
main = do
  prompt
  input <- T.pack <$> getLine
  let result = nf . desugar <$> parseLamExpr input
   in unless (input == ":q") $ printResult result >> main
