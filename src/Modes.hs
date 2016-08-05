{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
module Modes where

import Data.Char (isDigit)
import Data.List (intercalate)

import Control.Lens

import Control.Monad.State (evalStateT, execStateT, StateT, MonadState, get, put)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Maybe (runMaybeT, MaybeT)
import Control.Monad (MonadPlus, mzero)
import Control.Zipper.Simple

import Graphics.Vty
import qualified Language.Haskell.Exts as HSE

import View
import TextZipper

data Mod =
  Mod {
    _loc :: HSE.SrcSpanInfo,
    _modHead :: (Maybe (HSE.ModuleHead HSE.SrcSpanInfo)),
    _pragmas :: [HSE.ModulePragma HSE.SrcSpanInfo],
    _imports :: [HSE.ImportDecl HSE.SrcSpanInfo],
    _decls :: [HSE.Decl HSE.SrcSpanInfo]
  }
makeLenses ''Mod

type Decl = HSE.Decl HSE.SrcSpanInfo

data ModuleState h =
  ModuleState {
    _moduleStateFilename :: [Char],
    _moduleStateZipper :: h,
    _moduleStateDirty :: Bool
  }
makeLenses ''ModuleState

data TextState =
  TextState {
    _textStateFilename :: [Char],
    _textStateZipper :: TextZipper,
    _textStateLastSearch :: String,
    _textStateDirty :: Bool
  }
makeLenses ''TextState

textMode :: (MonadIO m1, MonadPlus m1) => Vty -> (Int, Int) -> TextState -> m1 a
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
    z <- use (_2 . textStateZipper)
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

  handleGoto :: (MonadState ((Int, Int), TextState) m, MonadIO m) => Int -> m ()
  handleGoto n = do
    pic <- generateWithStatus ("goto: " ++ if n == 0 then "" else show n)
    liftIO $ update vty pic

    e <- nextKeyEvent
    case e of
      (KChar c, []) | isDigit c -> handleGoto (n * 10 + read [c])
      (KBS, []) -> handleGoto (n `div` 10)
      (KEnter, []) -> (_2 . textStateZipper) %= goto n
      _ -> return ()

  handleSearch :: (MonadState ((Int, Int), TextState) m, MonadIO m) => String -> m ()
  handleSearch term = do
    pic <- generateWithStatus ("search: " ++ reverse term)
    liftIO $ update vty pic

    e <- nextKeyEvent
    case e of
      (KChar c, []) -> handleSearch (c:term)
      (KBS, []) -> handleSearch (drop 1 term)
      (KEnter, []) -> do
        let term' = reverse term
        (_2 . textStateLastSearch) .= term'
        (_2 . textStateZipper) %= search term'
      _ -> return ()

  switchToHaskellMode :: (MonadState ((Int, Int), TextState) m, MonadIO m, MonadPlus m) => m a
  switchToHaskellMode = do
    bounds <- use _1
    f <- use (_2 . textStateFilename)
    d <- use (_2 . textStateDirty)
    z <- use (_2 . textStateZipper)
    let contents = intercalate "\n" $ zipperLines z
    case HSE.parseFileContents contents of
      HSE.ParseOk (HSE.Module loc head pragmas imports decls) -> do
        let mod = Mod loc head pragmas imports decls
        haskellMode vty bounds $ ModuleState f (root mod) d
      HSE.ParseFailed loc message -> do
        pic <- generateWithStatus (show (HSE.srcLine loc) ++ ":" ++ show (HSE.srcColumn loc) ++ ":" ++ message)
        liftIO $ update vty pic
        handleNextKey
      _ -> do
        pic <- generateWithStatus "Can't edit HSX"
        liftIO $ update vty pic
        handleNextKey

  setDirty :: (MonadState (a, TextState) m) => m ()
  setDirty =
    (_2 . textStateDirty) .= True

  handleKeyEvent :: (MonadState ((Int, Int), TextState) m, MonadIO m, MonadPlus m) => (Key, [Modifier]) -> m a
  handleKeyEvent (KChar 'q', [MCtrl]) = do
    d <- use (_2 . textStateDirty)
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
    (_2 . textStateDirty) .= False
    z <- use (_2 . textStateZipper)
    name <- use (_2 . textStateFilename)
    liftIO $ writeFile name $ intercalate "\n" $ zipperLines z
    loop
  handleKeyEvent (KChar 'd', [MCtrl]) = do
    setDirty
    (_2 . textStateZipper) %= deleteLine
    loop
  handleKeyEvent (KChar '\t', []) = do
    setDirty
    (_2 . textStateZipper) %= indent
    loop
  handleKeyEvent (KBackTab, []) = do
    setDirty
    (_2 . textStateZipper) %= unindent
    loop
  handleKeyEvent (KChar 'c', [MCtrl]) = do
    setDirty
    (_2 . textStateZipper) %= commentOut
    loop
  handleKeyEvent (KChar 'g', [MCtrl]) = do
    setDirty
    (_2 . textStateZipper) %= uncommentOut
    loop
  handleKeyEvent (KChar 'l', [MCtrl]) = do
    handleGoto 0
    loop
  handleKeyEvent (KChar 'f', [MCtrl]) = do
    handleSearch []
    loop
  handleKeyEvent (KChar 'n', [MCtrl]) = do
    term <- use (_2 . textStateLastSearch)
    (_2 . textStateZipper) %= search term
    loop
  handleKeyEvent (KChar 'e', [MCtrl]) = do
    (_2 . textStateZipper) %= gotoLineEnd
    loop
  handleKeyEvent (KChar 'a', [MCtrl]) = do
    (_2 . textStateZipper) %= gotoLineStart
    loop
  handleKeyEvent (KChar 'h', [MMeta]) =
    switchToHaskellMode
  handleKeyEvent (KChar x, []) = do
    setDirty
    (_2 . textStateZipper) %= (insert x)
    loop
  handleKeyEvent (KUp, []) = do
    (_2 . textStateZipper) %= goUp
    loop
  handleKeyEvent (KDown, []) = do
    (_2 . textStateZipper) %= goDown
    loop
  handleKeyEvent (KLeft, []) = do
    (_2 . textStateZipper) %= goLeft
    loop
  handleKeyEvent (KRight, []) = do
    (_2 . textStateZipper) %= goRight
    loop
  handleKeyEvent (KBS, []) = do
    setDirty
    (_2 . textStateZipper) %= backspace
    loop
  handleKeyEvent (KDel, []) = do
    setDirty
    (_2 . textStateZipper) %= delete
    loop
  handleKeyEvent (KEnter, []) = do
    setDirty
    (_2 . textStateZipper) %= newline
    loop
  handleKeyEvent e = do
    liftIO $ print ("unknown event " ++ show e)
    loop

type HMMonad m = (MonadState ((Int, Int), ModuleState (Root Mod)) m, MonadIO m, MonadPlus m)

haskellMode :: (MonadIO m, MonadPlus m) => Vty -> (Int, Int) -> ModuleState (Root Mod) -> m a
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
    z <- use (_2 . moduleStateZipper)
    let Mod loc head pragmas imports decls = rezip z
    let mod = HSE.Module loc head pragmas imports decls
    let src = HSE.prettyPrint mod

    return $ (generateView (0, 0) height $ lines src) `addToTop` string defAttr "--- HASKELL ---"

  handleKeyEvent :: HMMonad m => (Key, [Modifier]) -> m a
  handleKeyEvent (KChar 'q', []) = mzero
  handleKeyEvent (KChar 'd', []) = do
    state <- get
    let z = view (_2 . moduleStateZipper) state
    case descendList $ descendLens decls z of
        Nothing -> loop
        Just z' -> do
          state' <- declMode vty $ set (_2 . moduleStateZipper) z' state
          put $ over (_2 . moduleStateZipper) (ascend . ascend) state'
          loop
  handleKeyEvent e = do
    liftIO $ print ("unknown key event: " ++ show e)
    loop

tug :: (a -> Maybe a) -> a -> a
tug f x =
  case f x of
    Nothing -> x
    Just x' -> x'

type MMonad z m = (
    MonadState ((Int, Int), ModuleState z) m,
    MonadIO m,
    MonadPlus m
  )

type DMMonad z m = (
    Rooted z,
    RootedAt z ~ Mod,
    MMonad (z =*=> Decl) m
  )

declMode ::
  (MonadIO m, MonadPlus m, Rooted z, RootedAt z ~ Mod) =>
  Vty -> ((Int, Int), ModuleState (z =*=> Decl)) -> m ((Int, Int), ModuleState (z =*=> Decl))
declMode vty (bounds, state) =
  execStateT loop (bounds, state)
 where
  loop :: DMMonad z m => m ()
  loop = do
    pic <- generatePicture
    liftIO $ update vty pic

    handleNextKeyEvent

  handleNextKeyEvent :: DMMonad z m => m ()
  handleNextKeyEvent =
    nextKeyEvent >>= handleKeyEvent

  nextKeyEvent :: DMMonad z m => m (Key, [Modifier])
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

  generatePicture :: DMMonad z m => m Picture
  generatePicture = do
    height <- use (_1 . _2)
    z <- use (_2 . moduleStateZipper)
    let Mod loc head pragmas imports decls = rezip z
    let mod = HSE.Module loc head pragmas imports decls
    let src = HSE.prettyPrint mod

    return $ (generateView (0, 0) height $ lines src) `addToTop` string defAttr "--- DECL ---"

  handleKeyEvent :: DMMonad z m => (Key, [Modifier]) -> m ()
  handleKeyEvent (KChar 'q', []) = mzero
  handleKeyEvent (KChar 'n', []) = do
    (_2 . moduleStateZipper) %= tug rightward
    loop
  handleKeyEvent (KChar 'p', []) = do
    (_2 . moduleStateZipper) %= tug leftward
    loop
  handleKeyEvent (KChar 'x', []) = do
    (_2 . moduleStateZipper) %= tug (either (const Nothing) Just . deleteFocus)
    loop
  handleKeyEvent (KChar 'u', []) =
    return ()
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
