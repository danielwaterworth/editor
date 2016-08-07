{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Printer where

import Control.Lens
import Control.Applicative
import Control.Monad.RWS

import Pretty

instance Printer (RWST () String Int Maybe) where
  s = tell
  highlight = id
  newline = do
    indent <- get
    tell $ '\n' : (take indent $ repeat ' ')

runPrinter :: RWST () String Int Maybe () -> Maybe String
runPrinter p = view _3 <$> runRWST p () 0

prettyPrint :: Pretty a => a -> Maybe String
prettyPrint = runPrinter . pretty
