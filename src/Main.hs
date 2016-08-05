module Main where

import Graphics.Vty
import Control.Exception (finally)
import Control.Monad.Trans.Maybe (runMaybeT)
import System.Environment (getArgs)

import Modes

main = do
  args <- getArgs
  state <- loadState args
  cfg <- standardIOConfig
  vty <- mkVty cfg
  bounds <- displayBounds $ outputIface vty

  finally
    (run vty $ state bounds)
    (shutdown vty)
