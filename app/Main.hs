{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.String                 (IsString (..))
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Data.Text.Prettyprint.Doc   (Pretty, pretty)
import           Data.Void
import           LamCalc.Untyped.DeBruijn    (desugar, nf)
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
       in (putStrLn . showResult) result >> main

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

showResult :: (Source s, Pretty a) => Either (ParseErrorBundle s Void) a -> String
showResult =
  \case
    Left error -> errorBundlePretty error
    Right result -> show $ pretty result

getLine' :: IsString s => IO (Maybe s)
getLine' =
  isEOF >>= \case
    True -> return Nothing
    False -> Just . fromString <$> getLine
