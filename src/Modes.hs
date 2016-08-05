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

loadState :: [String] -> IO (ModuleState (Root Mod))
loadState [filename] = do
  src <- readFile filename
  case HSE.parseFileContents src of
    HSE.ParseOk (HSE.Module a b c d e) -> return $ ModuleState filename (root $ Mod a b c d e) False
    _ -> undefined
