{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
module Modes where

import Data.Char (isDigit)
import Data.List (intercalate)
import Data.Typeable (Typeable)
import Data.Void
import Data.Proxy

import Control.Lens

import Control.Applicative

import Control.Monad.Reader (MonadReader, ask, runReaderT)
import Control.Monad.State (execStateT, runStateT, StateT, MonadState, get, put)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Maybe (runMaybeT, MaybeT)
import Control.Monad (MonadPlus, mzero)
import Control.Zipper.Simple

import Graphics.Vty
import Language.Haskell.Exts hiding (prettyPrint, Pretty)
import Language.Haskell.Exts.Prisms

import Pretty
import View
import HZipper
import TaggedZipper
import UnfocusedListZipper
import Printer

instance Wrapped (ModuleHead l) where
  type Unwrapped (ModuleHead l) = (l, ModuleName l, Maybe (WarningText l), Maybe (ExportSpecList l))

  _Wrapped' =
    iso
      (\(ModuleHead a b c d) -> (a, b, c, d))
      (\(a, b, c, d) -> ModuleHead a b c d)

instance Wrapped (ModuleName l) where
  type Unwrapped (ModuleName l) = (l, String)

  _Wrapped' =
    iso
      (\(ModuleName l s) -> (l, s))
      (\(l, s) -> ModuleName l s)

data State h =
  State {
    _bounds :: (Int, Int),
    _zipper :: h,
    _moduleStateDirty :: Bool
  }
makeLenses ''State

type MMonad m = (
    MonadIO m,
    MonadPlus m,
    MonadReader Vty m
  )

type ParentState mode = State (Zipper (ParentMode mode))

class EditorMode mode where
  type ParentMode mode
  type Zipper mode

  handleKeyEvent :: (EditorMode (ParentMode mode), MMonad m) => mode -> (Key, [Modifier]) -> State (Zipper mode) -> m (Either (ParentState mode) (State (Zipper mode)))

  modeOverlay :: (MMonad m, MonadState (State (Zipper mode)) m) => mode -> m Image

instance EditorMode Void where
  type ParentMode Void = Void
  type Zipper Void = Void

  handleKeyEvent = undefined
  modeOverlay = undefined

data RootMode = RootMode

instance EditorMode RootMode where
  type ParentMode RootMode = Void
  type Zipper RootMode = Root (Module ())

  handleKeyEvent _ (KChar 'q', []) _ = mzero
  handleKeyEvent _ (KChar 'h', []) s =
    Right <$>
      case descendPrism _Module' $ view zipper s of
        Just z ->
          runEditorMode (HaskellModuleMode RootMode) $ set zipper z s
        Nothing -> do
          liftIO $ print "Wrong type of module"
          return s
  handleKeyEvent _ e s = do
    liftIO $ print $ "unknown key event type " ++ show e
    return $ Right s

  modeOverlay _ = do
    height <- use (bounds . _2)
    return $ translateY (height - 1) $ string defAttr "Module"

data HaskellModuleMode mode = HaskellModuleMode mode

instance (EditorMode mode, Pretty (Zipper mode)) => EditorMode (HaskellModuleMode mode) where
  type ParentMode (HaskellModuleMode mode) = mode
  type Zipper (HaskellModuleMode mode) = Zipper mode ==> C_Module ()

  handleKeyEvent _ (KChar 'q', []) _ = mzero
  handleKeyEvent _ (KChar 't', []) s =
    return $ Left $ over zipper ascend s
  handleKeyEvent mode (KChar 'h', []) s =
    Right <$>
      runEditorMode
        (MaybeModuleHeadMode mode)
        (over zipper (hRightward . descendHList . descendLens (_Wrapped . from h5)) s)
  handleKeyEvent _ e s = do
    liftIO $ print $ "unknown key event type " ++ show e
    return $ Right s

  modeOverlay _ = do
    height <- use (bounds . _2)
    return $ translateY (height - 1) $ string defAttr "Haskell Module"

data MaybeModuleHeadMode x (l :: [*]) (r :: [*]) mode where
  MaybeModuleHeadMode :: (
      Ascend (HZipper (Zipper mode ==> x) l r (Maybe (ModuleHead ()))),
      BuildsOn (HZipper (Zipper mode ==> x) l r (Maybe (ModuleHead ()))) ~ (Zipper mode ==> x)
    ) => mode -> MaybeModuleHeadMode x l r mode

instance (EditorMode mode, Pretty (Zipper mode)) => EditorMode (MaybeModuleHeadMode x l r mode) where
  type ParentMode (MaybeModuleHeadMode x l r mode) = mode
  type Zipper (MaybeModuleHeadMode x l r mode) = HZipper (Zipper mode ==> x) l r (Maybe (ModuleHead ()))

  handleKeyEvent _ (KChar 'q', []) _ = mzero
  handleKeyEvent (MaybeModuleHeadMode _) (KChar 't', []) s =
    return $ Left $ over zipper (ascend . ascend) s
  handleKeyEvent mode@(MaybeModuleHeadMode _) (KChar 'h', []) s =
    Right <$>
      case descendPrism _Just $ view zipper s of
        Just z ->
          runEditorMode
            (ModuleHeadMode mode)
            (set zipper z s)
        Nothing -> do
          liftIO $ print "Can't descend into Nothing"
          return s
  handleKeyEvent _ (KChar 'X', []) s =
    return $ Right $ set (zipper . focus) Nothing s
  handleKeyEvent _ (KChar 'M', []) s =
    return $ Right $ set (zipper . focus) (Just (ModuleHead () (ModuleName () "") Nothing Nothing)) s
  handleKeyEvent _ e s = do
    liftIO $ print $ "unknown key event type " ++ show e
    return $ Right s

  modeOverlay _ = do
    height <- use (bounds . _2)
    return $ translateY (height - 1) $ string defAttr "Maybe Module Head"

data ModuleHeadMode mode = ModuleHeadMode mode

instance (EditorMode mode, Pretty (Zipper mode)) => EditorMode (ModuleHeadMode mode) where
  type ParentMode (ModuleHeadMode mode) = mode
  type Zipper (ModuleHeadMode mode) = (Zipper mode ==> ModuleHead ())

  handleKeyEvent _ (KChar 'q', []) _ = mzero
  handleKeyEvent _ (KChar 't', []) s =
    return $ Left $ over zipper ascend s
  handleKeyEvent mode@(ModuleHeadMode _) (KChar 'h', []) s =
    Right <$>
      runEditorMode
        (ModuleNameMode mode)
        (over zipper (hRightward . descendHList . descendLens (_Wrapped' . from h4)) s)
  handleKeyEvent _ e s = do
    liftIO $ print $ "unknown key event type " ++ show e
    return $ Right s

  modeOverlay _ = do
    height <- use (bounds . _2)
    return $ translateY (height - 1) $ string defAttr "Just ModuleHead"

data ModuleNameMode x (l :: [*]) (r :: [*]) mode where
  ModuleNameMode :: (
      Ascend (HZipper (Zipper mode ==> x) l r (ModuleName ())),
      BuildsOn (HZipper (Zipper mode ==> x) l r (ModuleName ())) ~ (Zipper mode ==> x)
    ) => mode -> ModuleNameMode x l r mode

instance (EditorMode mode, Pretty (Zipper mode)) => EditorMode (ModuleNameMode x l r mode) where
  type ParentMode (ModuleNameMode x l r mode) = mode
  type Zipper (ModuleNameMode x l r mode) = (HZipper (Zipper mode ==> x) l r (ModuleName ()))

  handleKeyEvent _ (KChar 'q', []) _ = mzero
  handleKeyEvent (ModuleNameMode _) (KChar 't', []) s =
    return $ Left $ over zipper (ascend . ascend) s
  handleKeyEvent mode@(ModuleNameMode _) (KChar 'h', []) s =
    Right <$>
      runEditorMode
        (StringMode mode)
        (over zipper (hRightward . descendHList . descendLens (_Wrapped' . from h2)) s)
  handleKeyEvent _ e s = do
    liftIO $ print $ "unknown key event type " ++ show e
    return $ Right s

  modeOverlay _ = do
    height <- use (bounds . _2)
    return $ translateY (height - 1) $ string defAttr "ModuleName"

data StringMode x l r mode where
  StringMode :: (
      Ascend (HZipper (Zipper mode ==> x) l r String),
      BuildsOn (HZipper (Zipper mode ==> x) l r String) ~ (Zipper mode ==> x)
    ) => mode -> StringMode x l r mode

instance (EditorMode mode, Pretty (Zipper mode)) => EditorMode (StringMode x l r mode) where
  type ParentMode (StringMode x l r mode) = mode
  type Zipper (StringMode x l r mode) = HZipper (Zipper mode ==> x) l r String

  handleKeyEvent _ (KChar 'q', []) _ = mzero
  handleKeyEvent (StringMode _) (KChar 't', []) s =
    return $ Left $ over zipper (ascend . ascend) s
  handleKeyEvent mode@(StringMode _) (KChar 'E', []) s =
    Right <$>
      runEditorMode
        (CharMode mode)
        (over zipper descendUList s)
  handleKeyEvent _ e s = do
    liftIO $ print $ "unknown key event type " ++ show e
    return $ Right s

  modeOverlay _ = do
    height <- use (bounds . _2)
    return $ translateY (height - 1) $ string defAttr "String"

data CharMode mode = CharMode mode

instance (EditorMode mode) => EditorMode (CharMode mode) where
  type ParentMode (CharMode mode) = mode
  type Zipper (CharMode mode) = (Zipper mode =%=> Char)

  handleKeyEvent _ (KChar 'q', [MCtrl]) _ = mzero
  handleKeyEvent _ (KChar 't', [MCtrl]) s =
    return $ Left $ over zipper ascend s
  handleKeyEvent _ (KBS, []) s =
    return $ Right $ over zipper backspace s
  handleKeyEvent _ (KChar c, []) s =
    return $ Right $ over zipper (insert c) s
  handleKeyEvent _ e s = do
    liftIO $ print $ "unknown key event type " ++ show e
    return $ Right s

  modeOverlay _ = do
    height <- use (bounds . _2)
    return $ translateY (height - 1) $ string defAttr "Char"

type InEditorMode z m = (MMonad m, EditorMode z, MonadState (State (Zipper z)) m)

nextKeyEvent :: (Pretty (Zipper z), InEditorMode z m) => z -> m (Key, [Modifier])
nextKeyEvent proxy = do
  vty <- ask
  e <- liftIO $ nextEvent vty
  case e of
    EvKey k m -> return (k, m)
    EvResize width height -> do
      bounds .= (width, height)
      picture <- generatePicture proxy
      liftIO $ update vty picture
      nextKeyEvent proxy
    _ -> do
      liftIO $ print ("unknown event type " ++ show e)
      nextKeyEvent proxy

generatePicture :: (Pretty (Zipper z), InEditorMode z m) => z -> m Picture
generatePicture proxy = do
  height <- use (bounds . _2)
  z <- use zipper
  case prettyPrint z of
    Right src -> do
      image <- modeOverlay proxy
      return $ (generateView (0, 0) height $ lines src) `addToTop` image
    Left err ->
      return $ picForImage $ string defAttr err

handleNextKeyEvent :: (Pretty (Zipper z), MMonad m, EditorMode z, EditorMode (ParentMode z)) => z -> State (Zipper z) -> m (Either (ParentState z) (State (Zipper z)))
handleNextKeyEvent proxy state =
  flip runStateT state (nextKeyEvent proxy) >>= uncurry (handleKeyEvent proxy)

runEditorMode :: (Pretty (Zipper z), MMonad m, EditorMode z, EditorMode (ParentMode z)) => z -> State (Zipper z) -> m (ParentState z)
runEditorMode proxy state = do
  vty <- ask
  state' <-
    flip execStateT state $ do
      pic <- generatePicture proxy
      liftIO $ update vty pic

  state'' <- handleNextKeyEvent proxy state'
  case state'' of
    Left x -> return x
    Right state''' -> runEditorMode proxy state'''

run :: Vty -> State (Root (Module ())) -> IO ()
run vty state = do
  runMaybeT $ flip runReaderT vty $ runEditorMode RootMode state
  return ()

loadState :: [String] -> IO ((Int, Int) -> State (Root (Module ())))
loadState [] =
  return $ \bounds -> State bounds (root $ Module () Nothing [] [] []) True
loadState [filename] = do
  src <- readFile filename
  case parseFileContents src of
    ParseOk m -> return $ \bounds -> State bounds (root $ fmap (const ()) m) False
    _ -> undefined
