{-# LANGUAGE TemplateHaskell #-}

import Graphics.Vty
import Data.List (foldr, intercalate)
import Control.Lens
import Control.Lens.TH
import Control.Exception (finally)
import System.Environment (getArgs)
import Text.Highlighting.Kate (highlightAs, TokenType(..))
import Control.Concurrent (threadDelay)

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

data State =
  State {
    _filename :: FilePath,
    _zipper :: Zipper
  }
makeLenses ''State

runMainLoop vty bounds state = do
  loop bounds state
 where
  loop bounds state = do
    render vty state

    e <- nextEvent vty
    handleEvent e
   where
    showN m n =
      let
        s = show n
      in
        (take (m - length s) $ repeat ' ') ++ s

    picture =
      let
        z = view zipper state
        lines = zipperLines z
        numLines = length lines

        lineNumWidth = length (show numLines)
        lineNumStyle = defAttr `withStyle` bold `withForeColor` yellow
        lineNumbers = foldr1 (<->) $ map (string lineNumStyle . showN lineNumWidth) [1..numLines]

        (x, y) = position z
        text = vertCat $ map (\x -> string defAttr " " <|> string defAttr x) lines
        image = lineNumbers <|> text

        height = snd bounds
        trimAmount = max 0 (y - (height `div` 2))

        image' = translateY (-trimAmount) image

        cursor = Cursor (x + lineNumWidth + 1) (y - trimAmount)
      in
        (picForImage image') { picCursor = cursor }

    render vty state =
      update vty picture

    handleEvent (EvKey (KChar 'q') [MCtrl]) =
      return ()
    handleEvent (EvKey (KChar 's') [MCtrl]) = do
      writeFile (view filename state) $ intercalate "\n" $ zipperLines $ view zipper state
      loop bounds state
    handleEvent (EvKey (KChar 'd') [MCtrl]) =
      loop bounds $ over zipper deleteLine state
    handleEvent (EvKey (KChar '\t') []) =
      loop bounds $ over zipper indent state
    handleEvent (EvKey KBackTab []) =
      loop bounds $ over zipper unindent state
    handleEvent (EvKey (KChar 'c') [MCtrl]) =
      loop bounds $ over zipper commentOut state
    handleEvent (EvKey (KChar 'g') [MCtrl]) =
      loop bounds $ over zipper uncommentOut state
    handleEvent (EvKey (KChar x) []) =
      loop bounds $ over zipper (insert x) state
    handleEvent (EvKey KUp []) =
      loop bounds $ over zipper goUp state
    handleEvent (EvKey KDown []) =
      loop bounds $ over zipper goDown state
    handleEvent (EvKey KLeft []) =
      loop bounds $ over zipper goLeft state
    handleEvent (EvKey KRight []) =
      loop bounds $ over zipper goRight state
    handleEvent (EvKey KBS []) =
      loop bounds $ over zipper backspace state
    handleEvent (EvKey KDel []) =
      loop bounds $ over zipper delete state
    handleEvent (EvKey KEnter []) =
      loop bounds $ over zipper newline state
    handleEvent (EvResize width height) =
      loop (width, height) state
    handleEvent e = do
      print ("unknown event " ++ show e)
      loop bounds state

loadState [filename] = do
  l <- lines <$> readFile filename
  return $ State filename $
    case l of
      [] -> Zipper [] [] [] []
      (x:xs) -> Zipper [] xs [] x

main = do
  args <- getArgs
  state <- loadState args
  cfg <- standardIOConfig
  vty <- mkVty cfg
  bounds <- displayBounds $ outputIface vty

  finally
    (runMainLoop vty bounds state)
    (shutdown vty)
