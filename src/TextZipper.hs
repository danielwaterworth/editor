{-# LANGUAGE TemplateHaskell #-}
module TextZipper where

import Data.List (isPrefixOf)

import Lens.Micro.Platform

data Zipper =
  Zipper {
    _linesAbove :: [String],
    _linesBelow :: [String],
    _charsLeft  :: [Char],
    _charsRight :: [Char]
  }
makeLenses ''Zipper

zipperLines :: Zipper -> [String]
zipperLines z =
  reverse (map reverse (view linesAbove z)) ++
  [reverse (view charsLeft z) ++ view charsRight z] ++
  view linesBelow z

position :: Zipper -> (Int, Int)
position z =
  (length $ view charsLeft z, length $ view linesAbove z)

insert :: Char -> Zipper -> Zipper
insert c =
  over charsLeft $ (:) c

backspace :: Zipper -> Zipper
backspace z =
  case view charsLeft z of
    "" ->
      case view linesAbove z of
        [] -> z
        (line : lines) -> set charsLeft line (set linesAbove lines z)
    (_:xs) -> set charsLeft xs z

delete :: Zipper -> Zipper
delete z =
  case view charsRight z of
    "" ->
      case view linesBelow z of
        [] -> z
        (line : lines) -> set charsRight line (set linesBelow lines z)
    (_:xs) -> set charsRight xs z

newline :: Zipper -> Zipper
newline z =
  let
    new = view charsLeft z
    z' = over linesAbove ((:) new) z
  in
    set charsLeft "" z'

goUp :: Zipper -> Zipper
goUp z =
  case view linesAbove z of
    [] -> z
    (c:cs) ->
      let
        leftChars = view charsLeft z
        rightChars = view charsRight z
        belowLines = view linesBelow z
        n = length leftChars
        c' = reverse c
        leftChars' = reverse $ take n c'
        rightChars' = drop n c'
        line = reverse leftChars ++ rightChars
      in
        Zipper cs (line : belowLines) leftChars' rightChars'

goDown :: Zipper -> Zipper
goDown z =
  case view linesBelow z of
    [] -> z
    (c:cs) ->
      let
        leftChars = view charsLeft z
        rightChars = view charsRight z
        aboveLines = view linesAbove z
        n = length leftChars
        leftChars' = reverse $ take n c
        rightChars' = drop n c
        line = reverse rightChars ++ leftChars
      in
        Zipper (line : aboveLines) cs leftChars' rightChars'

goLeft :: Zipper -> Zipper
goLeft z =
  case view charsLeft z of
    "" ->
      case view linesAbove z of
        [] -> z
        (line : lines) ->
          let
            rightChars = view charsRight z
            belowLines = view linesBelow z
          in
            Zipper lines (rightChars : belowLines) line []
    (c:cs) ->
       set charsLeft cs $ over charsRight ((:) c) z

goRight :: Zipper -> Zipper
goRight z =
  case view charsRight z of
    "" ->
      case view linesBelow z of
        [] -> z
        (line : lines) ->
          let
            leftChars = view charsLeft z
            aboveLines = view linesAbove z
          in
            Zipper (leftChars : aboveLines) lines [] line
    (c:cs) ->
       set charsRight cs $ over charsLeft ((:) c) z

deleteLineAbove :: Zipper -> Zipper
deleteLineAbove z =
  case view linesAbove z of
    [] -> z
    (line : lines) -> set linesAbove lines z

deleteLine :: Zipper -> Zipper
deleteLine =
  deleteLineAbove . goDown

indent :: Zipper -> Zipper
indent =
  over charsLeft (flip (++) "  ")

unindent :: Zipper -> Zipper
unindent z =
  case reverse (view charsLeft z) of
    (' ' : ' ' : xs) -> set charsLeft (reverse xs) z
    _ -> z

commentOut :: Zipper -> Zipper
commentOut =
  over charsLeft (flip (++) "--")

uncommentOut :: Zipper -> Zipper
uncommentOut z =
  case reverse (view charsLeft z) of
    ('-' : '-' : xs) -> set charsLeft (reverse xs) z
    _ -> z

gotoRelative :: Int -> Zipper -> Zipper
gotoRelative 0 z = z
gotoRelative n z | n > 0 =
  gotoRelative (n - 1) (goDown z)
gotoRelative n z | n < 0 =
  gotoRelative (n + 1) (goUp z)

goto :: Int -> Zipper -> Zipper
goto n z =
  let
    m = length (view linesAbove z) + 1
  in
    gotoRelative (n - m) z

match :: String -> Zipper -> Bool
match term z =
  term `isPrefixOf` view charsRight z

atLineStart :: Zipper -> Bool
atLineStart z =
  null (view charsLeft z)

atLineEnd :: Zipper -> Bool
atLineEnd z =
  null (view charsRight z)

atFileEnd :: Zipper -> Bool
atFileEnd z =
  null (view charsRight z) && null (view linesBelow z)

search :: String -> Zipper -> Zipper
search term z =
  let
    z' = goRight z
  in
    if atFileEnd z' || match term z' then
      z'
    else
      search term z'

gotoLineEnd :: Zipper -> Zipper
gotoLineEnd z =
  if atLineEnd z then
    z
  else
    gotoLineEnd $ goRight z

gotoLineStart :: Zipper -> Zipper
gotoLineStart z =
  if atLineStart z then
    z
  else
    gotoLineStart $ goLeft z