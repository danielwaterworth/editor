{-# LANGUAGE TemplateHaskell #-}
module TextZipper where

import Data.List (isPrefixOf)

import Lens.Micro.Platform

data TextZipper =
  TextZipper {
    _linesAbove :: [String],
    _linesBelow :: [String],
    _charsLeft  :: [Char],
    _charsRight :: [Char]
  }
makeLenses ''TextZipper

zipperLines :: TextZipper -> [String]
zipperLines z =
  reverse (map reverse (view linesAbove z)) ++
  [reverse (view charsLeft z) ++ view charsRight z] ++
  view linesBelow z

position :: TextZipper -> (Int, Int)
position z =
  (length $ view charsLeft z, length $ view linesAbove z)

insert :: Char -> TextZipper -> TextZipper
insert c =
  over charsLeft $ (:) c

backspace :: TextZipper -> TextZipper
backspace z =
  case view charsLeft z of
    "" ->
      case view linesAbove z of
        [] -> z
        (line : lines) -> set charsLeft line (set linesAbove lines z)
    (_:xs) -> set charsLeft xs z

delete :: TextZipper -> TextZipper
delete z =
  case view charsRight z of
    "" ->
      case view linesBelow z of
        [] -> z
        (line : lines) -> set charsRight line (set linesBelow lines z)
    (_:xs) -> set charsRight xs z

newline :: TextZipper -> TextZipper
newline z =
  let
    new = view charsLeft z
    z' = over linesAbove ((:) new) z
  in
    set charsLeft "" z'

goUp :: TextZipper -> TextZipper
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
        TextZipper cs (line : belowLines) leftChars' rightChars'

goDown :: TextZipper -> TextZipper
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
        TextZipper (line : aboveLines) cs leftChars' rightChars'

goLeft :: TextZipper -> TextZipper
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
            TextZipper lines (rightChars : belowLines) line []
    (c:cs) ->
       set charsLeft cs $ over charsRight ((:) c) z

goRight :: TextZipper -> TextZipper
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
            TextZipper (leftChars : aboveLines) lines [] line
    (c:cs) ->
       set charsRight cs $ over charsLeft ((:) c) z

deleteLineAbove :: TextZipper -> TextZipper
deleteLineAbove z =
  case view linesAbove z of
    [] -> z
    (line : lines) -> set linesAbove lines z

deleteLine :: TextZipper -> TextZipper
deleteLine =
  deleteLineAbove . goDown

indent :: TextZipper -> TextZipper
indent =
  over charsLeft (flip (++) "  ")

unindent :: TextZipper -> TextZipper
unindent z =
  case reverse (view charsLeft z) of
    (' ' : ' ' : xs) -> set charsLeft (reverse xs) z
    _ -> z

commentOut :: TextZipper -> TextZipper
commentOut =
  over charsLeft (flip (++) "--")

uncommentOut :: TextZipper -> TextZipper
uncommentOut z =
  case reverse (view charsLeft z) of
    ('-' : '-' : xs) -> set charsLeft (reverse xs) z
    _ -> z

gotoRelative :: Int -> TextZipper -> TextZipper
gotoRelative 0 z = z
gotoRelative n z | n > 0 =
  gotoRelative (n - 1) (goDown z)
gotoRelative n z | n < 0 =
  gotoRelative (n + 1) (goUp z)

goto :: Int -> TextZipper -> TextZipper
goto n z =
  let
    m = length (view linesAbove z) + 1
  in
    gotoRelative (n - m) z

match :: String -> TextZipper -> Bool
match term z =
  term `isPrefixOf` view charsRight z

atLineStart :: TextZipper -> Bool
atLineStart z =
  null (view charsLeft z)

atLineEnd :: TextZipper -> Bool
atLineEnd z =
  null (view charsRight z)

atFileEnd :: TextZipper -> Bool
atFileEnd z =
  null (view charsRight z) && null (view linesBelow z)

search :: String -> TextZipper -> TextZipper
search term z =
  let
    z' = goRight z
  in
    if atFileEnd z' || match term z' then
      z'
    else
      search term z'

gotoLineEnd :: TextZipper -> TextZipper
gotoLineEnd z =
  if atLineEnd z then
    z
  else
    gotoLineEnd $ goRight z

gotoLineStart :: TextZipper -> TextZipper
gotoLineStart z =
  if atLineStart z then
    z
  else
    gotoLineStart $ goLeft z
