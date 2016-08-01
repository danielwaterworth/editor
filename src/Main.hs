{-# LANGUAGE TemplateHaskell #-}

import Graphics.Vty
import Data.List (foldr, intercalate)
import Control.Lens
import Control.Lens.TH
import Control.Exception (finally)
import System.Environment (getArgs)
import Text.Highlighting.Kate (highlightAs, TokenType(..))

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

position :: Zipper -> Cursor
position z = 
  Cursor
    (length $ view charsLeft z)
    (length $ view linesAbove z)

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
    _ -> over charsLeft tail z

delete :: Zipper -> Zipper
delete z =
  case view charsRight z of
    "" ->
      case view linesBelow z of
        [] -> z
        (line : lines) -> set charsRight line (set linesBelow lines z)
    _ -> over charsRight tail z

newline :: Zipper -> Zipper
newline z =
  let
    new = view charsLeft z
    z' = over linesAbove ((:) new) z
  in
    set charsLeft "" z'

shiftCursorRight :: Int -> Cursor -> Cursor
shiftCursorRight _ NoCursor = NoCursor
shiftCursorRight n (Cursor l t) = Cursor (l + n) t

data State =
  State {
    _zipper :: Zipper
  }
makeLenses ''State

loop vty state = do
  render vty state

  e <- nextEvent vty
  handleEvent e
 where
  showN m n =
    let
      s = show n
    in
      (take (m - length s) $ repeat ' ') ++ s

  typeToStyle KeywordTok = defAttr `withForeColor` yellow `withStyle` bold
  typeToStyle DataTypeTok = defAttr `withForeColor` cyan `withStyle` bold
  typeToStyle CommentTok = defAttr `withForeColor` red
  typeToStyle OperatorTok = defAttr `withForeColor` yellow `withStyle` bold
  typeToStyle StringTok = defAttr `withForeColor` magenta
  typeToStyle _ = defAttr

  renderToken (t, s) =
    string (typeToStyle t) s

  renderLine line =
    string defAttr " " <|> (horizCat $ map renderToken line)

  picture =
    let
      z = view zipper state 
      lines = zipperLines z
      numLines = length lines

      lineNumWidth = length (show numLines)
      lineNumStyle = defAttr `withStyle` bold `withForeColor` yellow
      lineNumbers = foldr1 (<->) $ map (string lineNumStyle . showN lineNumWidth) [1..numLines]

      highlighted = highlightAs "haskell" $ intercalate "\n" lines

      text = vertCat $ map renderLine highlighted
    in
      (picForImage (lineNumbers <|> text)) { picCursor = shiftCursorRight (1 + lineNumWidth) $ position z }

  render vty state =
    update vty picture

  handleEvent (EvKey (KChar 'q') [MCtrl]) =
    return ()
  handleEvent (EvKey (KChar x) []) =
    loop vty $ over zipper (insert x) $ state
  handleEvent (EvKey KBS []) =
    loop vty $ over zipper backspace state
  handleEvent (EvKey KDel []) =
    loop vty $ over zipper delete state
  handleEvent (EvKey KEnter []) =
    loop vty $ over zipper newline state
  handleEvent e = do
    print ("unknown event " ++ show e)
    loop vty state

loadState [] =
  return $ State $ Zipper [] [] [] []
loadState [filename] = do
  l <- lines <$> readFile filename
  return $
    case l of
      [] -> State $ Zipper [] [] [] []
      (x:xs) -> State $ Zipper [] xs [] x

main = do
  args <- getArgs
  state <- loadState args
  cfg <- standardIOConfig
  vty <- mkVty cfg
   
  finally
    (loop vty state)
    (shutdown vty)
