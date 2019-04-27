{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.String                 (IsString (..))
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Data.Text.Prettyprint.Doc   (Pretty, pretty)
import           Data.Void
import           LamCalc.Untyped.Naive       (desugar, nf)
import           LamCalc.Untyped.Parser
import qualified LamCalc.Untyped.Parser.Expr as P
import           System.IO                   (hFlush, isEOF, stdout)
import           Text.Megaparsec             (ParseErrorBundle, errorBundlePretty, parse)

data Request
  = Quit
  | Run Text

main :: IO ()
main =
  getRequest >>= \case
    Quit -> return ()
    Run s ->
      let result = nf . desugar <$> parseLamExpr s
       in printResult result >> main

getRequest :: IO Request
getRequest = do
  prompt
  makeRequest <$> getLine'
  where
    prompt = putStr "λ> " >> hFlush stdout
    makeRequest =
      \case
        Nothing -> Quit
        Just s
          | T.strip s == ":q" -> Quit
          | otherwise -> Run s

parseLamExpr :: Source s => s -> Either (ParseErrorBundle s Void) (P.Expr s)
parseLamExpr = parse expr "stdin"

printResult :: (Source s, Pretty a) => Either (ParseErrorBundle s Void) a -> IO ()
printResult =
  \case
    Left error -> putStrLn $ errorBundlePretty error
    Right result -> print $ pretty result

getLine' :: IsString s => IO (Maybe s)
getLine' =
  isEOF >>= \case
    True -> return Nothing
    False -> Just . fromString <$> getLine
