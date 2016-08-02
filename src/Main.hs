{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}

import Graphics.Vty
import Data.Char (isDigit)
import Data.List (foldr, intercalate, isPrefixOf)
import Control.Lens
import Control.Lens.TH
import Control.Exception (finally)
import System.Environment (getArgs)
import Text.Highlighting.Kate (highlightAs, TokenType(..))
import Control.Concurrent (threadDelay)
import Control.Monad.State (execStateT, StateT, MonadState)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Maybe (runMaybeT, MaybeT)
import Control.Monad (MonadPlus, mzero)

import Zipper

data State =
  State {
    _filename :: FilePath,
    _zipper :: Zipper,
    _lastSearch :: String,
    _dirty :: Bool
  }
makeLenses ''State

runMainLoop vty bounds state = do
  runMaybeT $ flip execStateT (bounds, state) loop
 where
  loop :: StateT ((Int, Int), State) (MaybeT IO) ()
  loop = do
    render

    e <- nextKeyEvent
    handleKeyEvent e
    loop
   where
    showN m n =
      let
        s = show n
      in
        (take (m - length s) $ repeat ' ') ++ s

    generatePicture :: (MonadState ((Int, Int), State) m) => m Picture
    generatePicture = do
      z <- use (_2 . zipper)
      let lines = zipperLines z
      let numLines = length lines

      let lineNumWidth = length (show numLines)
      let lineNumStyle = defAttr `withStyle` bold `withForeColor` yellow
      let lineNumbers = foldr1 (<->) $ map (string lineNumStyle . showN lineNumWidth) [1..numLines]

      let (x, y) = position z
      let text = vertCat $ map (\x -> string defAttr " " <|> string defAttr x) lines
      let image = lineNumbers <|> text

      let height = snd bounds
      let trimAmount = max 0 (y - (height `div` 2))

      let image' = translateY (-trimAmount) image

      let cursor = Cursor (x + lineNumWidth + 1) (y - trimAmount)
      return (picForImage image') { picCursor = cursor }

    render :: (MonadState ((Int, Int), State) m, MonadIO m) => m ()
    render = do
      picture <- generatePicture
      liftIO $ update vty picture

    renderWithStatus status = do
      (width, height) <- use _1
      picture <- generatePicture
      let statusBar = translateY (height - 1) $ string defAttr (status ++ (take width $ repeat ' '))
      let cursor = Cursor (length status) (height - 1)
      liftIO $ update vty (picture `addToTop` statusBar) { picCursor = cursor }

    nextKeyEvent :: (MonadState ((Int, Int), State) m, MonadIO m) => m (Key, [Modifier])
    nextKeyEvent = do
      e <- liftIO $ nextEvent vty
      case e of
        EvKey k m -> return (k, m)
        EvResize width height -> do
          _1 .= (width, height)
          render
          nextKeyEvent
        _ -> do
          liftIO $ print ("unknown event type " ++ show e)
          nextKeyEvent

    handleGoto n = do
      renderWithStatus ("goto: " ++ if n == 0 then "" else show n)

      e <- nextKeyEvent
      case e of
        (KChar c, []) | isDigit c -> handleGoto (n * 10 + read [c])
        (KBS, []) -> handleGoto (n `div` 10)
        (KEnter, []) -> (_2 . zipper) %= goto n
        _ -> return ()

    handleSearch term = do
      renderWithStatus ("search: " ++ reverse term)

      e <- nextKeyEvent
      case e of
        (KChar c, []) -> handleSearch (c:term)
        (KBS, []) -> handleSearch (drop 1 term)
        (KEnter, []) -> do
          let term' = reverse term
          (_2 . lastSearch) .= term'
          (_2 . zipper) %= search term'
        _ -> return ()

    setDirty :: (MonadState (a, State) m) => m ()
    setDirty =
      (_2 . dirty) .= True

    handleKeyEvent :: (MonadState ((Int, Int), State) m, MonadIO m, MonadPlus m) => (Key, [Modifier]) -> m ()
    handleKeyEvent (KChar 'q', [MCtrl]) = do
      d <- use (_2 . dirty)
      if not d then
        mzero
       else do
        renderWithStatus "NOT SAVED, '!' to exit"
        e <- nextKeyEvent
        case e of
          (KChar '!', []) -> mzero
          _ -> return ()
    handleKeyEvent (KChar 's', [MCtrl]) = do
      (_2 . dirty) .= False
      z <- use (_2 . zipper)
      name <- use (_2 . filename)
      liftIO $ writeFile name $ intercalate "\n" $ zipperLines z
    handleKeyEvent (KChar 'd', [MCtrl]) = do
      setDirty
      (_2 . zipper) %= deleteLine
    handleKeyEvent (KChar '\t', []) = do
      setDirty
      (_2 . zipper) %= indent
    handleKeyEvent (KBackTab, []) = do
      setDirty
      (_2 . zipper) %= unindent
    handleKeyEvent (KChar 'c', [MCtrl]) = do
      setDirty
      (_2 . zipper) %= commentOut
    handleKeyEvent (KChar 'g', [MCtrl]) = do
      setDirty
      (_2 . zipper) %= uncommentOut
    handleKeyEvent (KChar 'l', [MCtrl]) =
      handleGoto 0
    handleKeyEvent (KChar 'f', [MCtrl]) =
      handleSearch []
    handleKeyEvent (KChar 'n', [MCtrl]) = do
      term <- use (_2 . lastSearch)
      (_2 . zipper) %= search term
    handleKeyEvent (KChar 'e', [MCtrl]) =
      (_2 . zipper) %= gotoLineEnd
    handleKeyEvent (KChar 'a', [MCtrl]) =
      (_2 . zipper) %= gotoLineStart
    handleKeyEvent (KChar x, []) = do
      setDirty
      (_2 . zipper) %= (insert x)
    handleKeyEvent (KUp, []) =
      (_2 . zipper) %= goUp
    handleKeyEvent (KDown, []) =
      (_2 . zipper) %= goDown
    handleKeyEvent (KLeft, []) =
      (_2 . zipper) %= goLeft
    handleKeyEvent (KRight, []) =
      (_2 . zipper) %= goRight
    handleKeyEvent (KBS, []) = do
      setDirty
      (_2 . zipper) %= backspace
    handleKeyEvent (KDel, []) = do
      setDirty
      (_2 . zipper) %= delete
    handleKeyEvent (KEnter, []) = do
      setDirty
      (_2 . zipper) %= newline
    handleKeyEvent e = do
      liftIO $ print ("unknown event " ++ show e)

loadState [filename] = do
  l <- lines <$> readFile filename
  let zipper =
        case l of
          [] -> Zipper [] [] [] []
          (x:xs) -> Zipper [] xs [] x
  return $ State filename zipper "" False

main = do
  args <- getArgs
  state <- loadState args
  cfg <- standardIOConfig
  vty <- mkVty cfg
  bounds <- displayBounds $ outputIface vty

  finally
    (runMainLoop vty bounds state)
    (shutdown vty)
