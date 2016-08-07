{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Printer where

import Control.Lens
import Control.Applicative
import Control.Monad.RWS

import Pretty

instance Printer (RWST () String Int (Either String)) where
  s = tell
  highlight = id
  newline = do
    indent <- get
    tell $ '\n' : (take indent $ repeat ' ')

runPrinter :: RWST () String Int (Either String) () -> Either String String
runPrinter p = view _3 <$> runRWST p () 0

prettyPrint :: Pretty a => a -> Either String String
prettyPrint = runPrinter . pretty
