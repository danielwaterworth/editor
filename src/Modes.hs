{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
module Modes where

import Data.Char (isDigit)
import Data.List (intercalate)

import Lens.Micro.Platform

import Control.Monad.State (evalStateT, StateT, MonadState)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Maybe (runMaybeT, MaybeT)
import Control.Monad (MonadPlus, mzero)
import Control.Zipper hiding (zipper)
import qualified Control.Zipper as Z

import Graphics.Vty
import Language.Haskell.Exts

import View
import TextZipper

data Mod =
  Mod
    SrcSpanInfo
    (Maybe (ModuleHead SrcSpanInfo))
    [ModulePragma SrcSpanInfo]
    [ImportDecl SrcSpanInfo]
    [Decl SrcSpanInfo]

data ModuleState h =
  ModuleState {
    _moduleStateFilename :: [Char],
    _moduleStateZipper :: h,
    _moduleStateComments :: [Comment],
    _moduleStateDirty :: Bool
  }
makeFields ''ModuleState

data TextState =
  TextState {
    _textStateFilename :: [Char],
    _textStateZipper :: TextZipper,
    _textStateLastSearch :: String,
    _textStateDirty :: Bool
  }
makeFields ''TextState

textMode :: (MonadIO m, MonadPlus m) => Vty -> (Int, Int) -> TextState -> m a
textMode vty bounds state =
  evalStateT loop (bounds, state)
 where
  loop :: (MonadState ((Int, Int), TextState) m, MonadIO m, MonadPlus m) => m a
  loop = do
    picture <- generatePicture
    liftIO $ update vty picture

    handleNextKey

  handleNextKey :: (MonadState ((Int, Int), TextState) m, MonadIO m, MonadPlus m) => m a
  handleNextKey =
    nextKeyEvent >>= handleKeyEvent

  generatePicture :: (MonadState ((Int, Int), TextState) m) => m Picture
  generatePicture = do
    z <- use (_2 . zipper)
    height <- use (_1 . _2)
    return (generateView (position z) height (zipperLines z))

  generateWithStatus :: (MonadState ((Int, Int), TextState) m) => String -> m Picture
  generateWithStatus status = do
    (width, height) <- use _1
    picture <- generatePicture
    let statusBar = translateY (height - 1) $ string defAttr (status ++ (take width $ repeat ' '))
    let cursor = Cursor (length status) (height - 1)
    
    return (picture `addToTop` statusBar) { picCursor = cursor }

  nextKeyEvent :: (MonadState ((Int, Int), TextState) m, MonadIO m) => m (Key, [Modifier])
  nextKeyEvent = do
    e <- liftIO $ nextEvent vty
    case e of
      EvKey k m -> return (k, m)
      EvResize width height -> do
        _1 .= (width, height)
        picture <- generatePicture
        liftIO $ update vty picture
        nextKeyEvent
      _ -> do
        liftIO $ print ("unknown event type " ++ show e)
        nextKeyEvent

  handleGoto n = do
    pic <- generateWithStatus ("goto: " ++ if n == 0 then "" else show n)
    liftIO $ update vty pic

    e <- nextKeyEvent
    case e of
      (KChar c, []) | isDigit c -> handleGoto (n * 10 + read [c])
      (KBS, []) -> handleGoto (n `div` 10)
      (KEnter, []) -> (_2 . zipper) %= goto n
      _ -> return ()

  handleSearch term = do
    pic <- generateWithStatus ("search: " ++ reverse term)
    liftIO $ update vty pic

    e <- nextKeyEvent
    case e of
      (KChar c, []) -> handleSearch (c:term)
      (KBS, []) -> handleSearch (drop 1 term)
      (KEnter, []) -> do
        let term' = reverse term
        (_2 . lastSearch) .= term'
        (_2 . zipper) %= search term'
      _ -> return ()

  switchToHaskellMode :: (MonadState ((Int, Int), TextState) m, MonadIO m, MonadPlus m) => m a
  switchToHaskellMode = do
    bounds <- use _1
    f <- use (_2 . filename)
    d <- use (_2 . dirty)
    z <- use (_2 . zipper)
    let contents = intercalate "\n" $ zipperLines z
    case parseFileContentsWithComments defaultParseMode contents of
      ParseOk (Module loc head pragmas imports decls, comments) -> do
        let mod = Mod loc head pragmas imports decls
        haskellMode vty bounds $ ModuleState f (Z.zipper mod) comments d
      ParseFailed loc message -> do
        pic <- generateWithStatus (show (srcLine loc) ++ ":" ++ show (srcColumn loc) ++ ":" ++ message)
        liftIO $ update vty pic
        handleNextKey
      _ -> do
        pic <- generateWithStatus "Can't edit HSX"
        liftIO $ update vty pic
        handleNextKey

  setDirty :: (MonadState (a, TextState) m) => m ()
  setDirty =
    (_2 . dirty) .= True

  handleKeyEvent :: (MonadState ((Int, Int), TextState) m, MonadIO m, MonadPlus m) => (Key, [Modifier]) -> m a
  handleKeyEvent (KChar 'q', [MCtrl]) = do
    d <- use (_2 . dirty)
    if not d then
      mzero
     else do
      pic <- generateWithStatus "NOT SAVED, '!' to exit"
      liftIO $ update vty pic
      e <- nextKeyEvent
      case e of
        (KChar '!', []) -> mzero
        _ -> loop
  handleKeyEvent (KChar 's', [MCtrl]) = do
    (_2 . dirty) .= False
    z <- use (_2 . zipper)
    name <- use (_2 . filename)
    liftIO $ writeFile name $ intercalate "\n" $ zipperLines z
    loop
  handleKeyEvent (KChar 'd', [MCtrl]) = do
    setDirty
    (_2 . zipper) %= deleteLine
    loop
  handleKeyEvent (KChar '\t', []) = do
    setDirty
    (_2 . zipper) %= indent
    loop
  handleKeyEvent (KBackTab, []) = do
    setDirty
    (_2 . zipper) %= unindent
    loop
  handleKeyEvent (KChar 'c', [MCtrl]) = do
    setDirty
    (_2 . zipper) %= commentOut
    loop
  handleKeyEvent (KChar 'g', [MCtrl]) = do
    setDirty
    (_2 . zipper) %= uncommentOut
    loop
  handleKeyEvent (KChar 'l', [MCtrl]) = do
    handleGoto 0
    loop
  handleKeyEvent (KChar 'f', [MCtrl]) = do
    handleSearch []
    loop
  handleKeyEvent (KChar 'n', [MCtrl]) = do
    term <- use (_2 . lastSearch)
    (_2 . zipper) %= search term
    loop
  handleKeyEvent (KChar 'e', [MCtrl]) = do
    (_2 . zipper) %= gotoLineEnd
    loop
  handleKeyEvent (KChar 'a', [MCtrl]) = do
    (_2 . zipper) %= gotoLineStart
    loop
  handleKeyEvent (KChar 'h', [MMeta]) =
    switchToHaskellMode
  handleKeyEvent (KChar x, []) = do
    setDirty
    (_2 . zipper) %= (insert x)
    loop
  handleKeyEvent (KUp, []) = do
    (_2 . zipper) %= goUp
    loop
  handleKeyEvent (KDown, []) = do
    (_2 . zipper) %= goDown
    loop
  handleKeyEvent (KLeft, []) = do
    (_2 . zipper) %= goLeft
    loop
  handleKeyEvent (KRight, []) = do
    (_2 . zipper) %= goRight
    loop
  handleKeyEvent (KBS, []) = do
    setDirty
    (_2 . zipper) %= backspace
    loop
  handleKeyEvent (KDel, []) = do
    setDirty
    (_2 . zipper) %= delete
    loop
  handleKeyEvent (KEnter, []) = do
    setDirty
    (_2 . zipper) %= newline
    loop
  handleKeyEvent e = do
    liftIO $ print ("unknown event " ++ show e)
    loop

type HMMonad m = (MonadState ((Int, Int), ModuleState (Top :>> Mod)) m, MonadIO m, MonadPlus m)

haskellMode :: (MonadIO m, MonadPlus m) => Vty -> (Int, Int) -> ModuleState (Top :>> Mod) -> m a
haskellMode vty bounds state =
  evalStateT loop (bounds, state)
 where
  loop :: HMMonad m => m a
  loop = do
    pic <- generatePicture
    liftIO $ update vty pic

    handleNextKeyEvent

  handleNextKeyEvent :: HMMonad m => m a
  handleNextKeyEvent =
    nextKeyEvent >>= handleKeyEvent

  nextKeyEvent :: HMMonad m => m (Key, [Modifier])
  nextKeyEvent = do
    e <- liftIO $ nextEvent vty
    case e of
      EvKey k m -> return (k, m)
      EvResize width height -> do
        _1 .= (width, height)
        picture <- generatePicture
        liftIO $ update vty picture
        nextKeyEvent
      _ -> do
        liftIO $ print ("unknown event type " ++ show e)
        nextKeyEvent

  generatePicture :: HMMonad m => m Picture
  generatePicture = do
    height <- use (_1 . _2)
    z <- use (_2 . zipper)
    let Mod loc head pragmas imports decls  = rezip z
    let mod = Module loc head pragmas imports decls
    comments <- use (_2 . comments)
    let src = exactPrint mod comments

    return $ generateView (0, 0) height $ lines src

  handleKeyEvent :: HMMonad m => (Key, [Modifier]) -> m a
  handleKeyEvent (KChar 'q', [MCtrl]) =
    mzero
  handleKeyEvent e = do
    liftIO $ print ("unknown key event: " ++ show e)
    loop

loadState [filename] = do
  l <- lines <$> readFile filename
  let zipper =
        case l of

          [] -> TextZipper [] [] [] []
          (x:xs) -> TextZipper [] xs [] x
  return $ TextState filename zipper "" False
