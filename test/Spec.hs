module Main where

import Language.Haskell.Exts hiding (prettyPrint)
import Printer
import Data.Maybe

import Test.Hspec

parse' :: String -> Module ()
parse' x = fmap (const ()) $ fromParseResult $ parseFileContents x

same :: String -> Expectation
same x = shouldBe (fromJust $ prettyPrint $ parse' x) x

main :: IO ()
main = hspec $
  describe "pretty printing" $
    it "should pretty out the module head" $
      same "module Foo where"
