{-# LANGUAGE OverloadedStrings #-}

module SExp where

import Control.Applicative
import Data.Char
import Data.List (intersperse)
import Data.Monoid
import Data.Text (pack, unpack, Text)
import Data.Text.IO
import Prelude hiding (readFile)
import Text.Trifecta hiding (spaces, newline)

data OneLineSExpr =
  Atom Text |
  OneLineList [OneLineSExpr]
    deriving Show

data SExpr =
  OneLine OneLineSExpr |
  MultiLine [OneLineSExpr] [SExpr]
    deriving Show

whitespace :: Parser ()
whitespace = many (satisfy isSpace) >> pure ()

spaces :: Parser ()
spaces = many (text " ") >> pure ()

newline :: Parser ()
newline = text "\n" >> spaces

oneLineList :: Parser OneLineSExpr
oneLineList = do
  text "("
  xs <-
    many $ do
      spaces
      oneLineExpr
  oneLineListEnd xs

oneLineListEnd :: [OneLineSExpr] -> Parser OneLineSExpr
oneLineListEnd xs = do
  text ")"
  pure $ OneLineList xs

multiLineListEnd :: [OneLineSExpr] -> Parser SExpr
multiLineListEnd xs = do
  ys <-
    many $ do
      newline
      sexpr
  text ")"
  pure $ MultiLine xs ys

listEnd :: [OneLineSExpr] -> Parser SExpr
listEnd xs =
  (OneLine <$> oneLineListEnd xs) <|> multiLineListEnd xs

oneLineExpr :: Parser OneLineSExpr
oneLineExpr =
  oneLineList <|> atom

symbols :: [Char]
symbols =
  "-_=<>\\:"

atom :: Parser OneLineSExpr
atom =
  (Atom . pack) <$> some (satisfy (\x -> elem x symbols || isAlphaNum x))

list :: Parser SExpr
list = do
  text "("
  xs <-
    many $ do
      spaces
      oneLineExpr
  listEnd xs

sexpr :: Parser SExpr
sexpr =
  (OneLine <$> atom) <|> list

document :: Parser [SExpr]
document = do
  x <- many sexpr
  whitespace
  eof
  pure x

parseFile :: FilePath -> IO (Maybe [SExpr])
parseFile filename =
  parseFromFile document filename
