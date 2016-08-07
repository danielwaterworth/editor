module Main where

import Language.Haskell.Exts hiding (prettyPrint)
import Printer
import Data.Maybe

import Test.Hspec

parse' :: String -> Module ()
parse' x = fmap (const ()) $ fromParseResult $ parseFileContents x

same :: String -> Expectation
same x = shouldBe (prettyPrint $ parse' x) (Just x)

main :: IO ()
main = hspec $
  describe "pretty printing" $ do
    it "works on an empty file" $
      same ""

    it "works with a module head" $
      same "module Foo where"

    describe "imports" $ do
      it "works with an import" $
        same "import Foo"

      it "works with two imports" $
        same "import Foo\nimport Bar"

      it "works with a qualified import" $
        same "import qualified Foo"

      it "should separate the head from the first import with an empty line" $
        same "module Main where\n\nimport Foo"

      it "works with a named import" $
        same "import Foo as F"

      it "works with a spec list with a single item" $
        same "import Foo (foo)"

      it "works with a spec list with two items" $
        same "import Foo (foo, bar)"

      it "works with a hiding clause" $
        same "import Foo hiding (foo)"

    describe "a decl" $
      describe "for a type" $
        it "works in a simple case" $
          same "a :: Bool"
