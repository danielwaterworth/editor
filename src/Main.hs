{-# LANGUAGE TemplateHaskell #-}

import Graphics.Vty
import Data.List (foldr)
import Control.Lens
import Control.Lens.TH

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
  reverse (view linesAbove z) ++ [reverse (view charsLeft z) ++ view charsRight z] ++ view linesBelow z

insert :: Char -> Zipper -> Zipper
insert c =
  over charsLeft $ (:) c

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
    picForImage $ foldr1 (<->) $ map (string defAttr . ((:) ' ')) $ zipperLines $ view zipper state

  render vty state =
    update vty picture

  handleEvent (EvKey (KChar 'q') [MCtrl]) =
    shutdown vty
  handleEvent (EvKey (KChar x) []) =
    loop vty $ over zipper (insert x) $ state
  handleEvent e =
    loop vty state

main = do
  cfg <- standardIOConfig
  vty <- mkVty cfg
   
  loop vty $ State $ Zipper ["first line"] ["last line"] [] []
