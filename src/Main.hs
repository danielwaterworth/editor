import Graphics.Vty

import qualified Data.Text as T
import qualified Data.Text.Zipper as Z

data State =
  State (Z.TextZipper T.Text)

loop vty state = do
  render vty state

  e <- nextEvent vty
  handleEvent e
 where
  picture =
    let
      line0 = string (defAttr ` withForeColor ` green) "first line"
      line1 = string (defAttr ` withBackColor ` blue) "second line"
      img = line0 <-> line1
    in
      picForImage img

  render vty state =
    update vty picture

  handleEvent (EvKey (KChar 'q') [MCtrl]) =
    shutdown vty
  handleEvent e =
    loop vty state

main = do
  cfg <- standardIOConfig
  vty <- mkVty cfg
   
  loop vty $ State $ Z.textZipper [] Nothing
