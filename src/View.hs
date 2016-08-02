{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}
module View where

import Graphics.Vty
import Control.Monad.State (execStateT, StateT, MonadState)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Maybe (runMaybeT, MaybeT)
import Control.Monad (MonadPlus, mzero)

generateView :: (Int, Int) -> Int -> [String] -> Picture
generateView (x, y) height lines =
  let
    numLines = length lines

    lineNumWidth = length (show numLines)
    lineNumStyle = defAttr `withStyle` bold `withForeColor` yellow
    lineNumbers = foldr1 (<->) $ map (string lineNumStyle . showN lineNumWidth) [1..numLines]

    text = vertCat $ map (\x -> string defAttr " " <|> string defAttr x) lines
    image = lineNumbers <|> text

    trimAmount = max 0 (y - (height `div` 2))
    image' = translateY (-trimAmount) image

    cursor = Cursor (x + lineNumWidth + 1) (y - trimAmount)
  in
    (picForImage image') { picCursor = cursor }
 where
  showN m n =
    let
      s = show n
    in
      (take (m - length s) $ repeat ' ') ++ s
