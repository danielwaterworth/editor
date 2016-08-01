{-# LANGUAGE TemplateHaskell #-}

import Graphics.Vty
import Data.List (foldr)
import Control.Lens
import Control.Lens.TH
import Control.Exception (finally)

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
  picture =
    let
      z = view zipper state 
      lines = zipperLines z 
      image = foldr1 (<->) $ map (string defAttr . ((:) ' ')) lines 
    in
      (picForImage image) { picCursor = shiftCursorRight 1 $ position z }

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

main = do
  cfg <- standardIOConfig
  vty <- mkVty cfg
   
  finally
    (loop vty $ State $ Zipper [reverse "first line"] ["last line"] [] [])
    (shutdown vty)
