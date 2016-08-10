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

fromRight :: Either a b -> b
fromRight (Right x) = x
fromRight _ = error "fromRight"

tug :: (a -> Maybe a) -> a -> a
tug f x =
  case f x of
    Nothing -> x
    Just x' -> x'

type MMonad z m = (
    Pretty z,
    MonadIO m,
    MonadPlus m,
    MonadReader Vty m
  )

class EditorMode z where
  type ParentMode z

  handleKeyEvent :: MMonad z m => (Key, [Modifier]) -> State z -> m (Either (State (ParentMode z)) (State z))

  modeOverlay :: (MMonad z m, MonadState (State z) m) => m Image

instance EditorMode (Root (Module ())) where
  type ParentMode (Root (Module ())) = Void

  handleKeyEvent (KChar 'q', []) _ = mzero
  handleKeyEvent (KChar 'h', []) s =
    Right <$>
      case descendPrism _Module' $ view zipper s of
        Just z ->
          runEditorMode $ set zipper z s
        Nothing -> do
          liftIO $ print "Wrong type of module"
          return s
  handleKeyEvent e s = do
    liftIO $ print $ "unknown key event type " ++ show e
    return $ Right s

  modeOverlay = do
    height <- use (bounds . _2)
    return $ translateY (height - 1) $ string defAttr "Module"

instance Pretty z => EditorMode (z ==> C_Module ()) where
  type ParentMode (z ==> C_Module ()) = z

  handleKeyEvent (KChar 'q', []) _ = mzero
  handleKeyEvent (KChar 't', []) s =
    return $ Left $ over zipper ascend s
  handleKeyEvent (KChar 'h', []) s =
    Right <$> runEditorMode (over zipper (hRightward . descendHList . descendLens (_Wrapped . from h5)) s)
  handleKeyEvent e s = do
    liftIO $ print $ "unknown key event type " ++ show e
    return $ Right s

  modeOverlay = do
    height <- use (bounds . _2)
    return $ translateY (height - 1) $ string defAttr "Haskell Module"

instance (
      Pretty z,
      Ascend (HZipper (z ==> x) l r (Maybe (ModuleHead ()))),
      BuildsOn (HZipper (z ==> x) l r (Maybe (ModuleHead ()))) ~ (z ==> x)
    ) =>
      EditorMode (HZipper (z ==> x) l r (Maybe (ModuleHead ())))
    where
  type ParentMode (HZipper (z ==> x) l r (Maybe (ModuleHead ()))) = z

  handleKeyEvent (KChar 'q', []) _ = mzero
  handleKeyEvent (KChar 't', []) s =
    return $ Left $ over zipper (ascend . ascend) s
  handleKeyEvent (KChar 'h', []) s =
    Right <$>
      case descendPrism _Just $ view zipper s of
        Just z -> runEditorMode $ set zipper z s
        Nothing -> do
          liftIO $ print "Can't descend into Nothing"
          return s
  handleKeyEvent (KChar 'X', []) s =
    return $ Right $ set (zipper . focus) Nothing s
  handleKeyEvent (KChar 'M', []) s =
    return $ Right $ set (zipper . focus) (Just (ModuleHead () (ModuleName () "Foo") Nothing Nothing)) s
  handleKeyEvent e s = do
    liftIO $ print $ "unknown key event type " ++ show e
    return $ Right s

  modeOverlay = do
    height <- use (bounds . _2)
    return $ translateY (height - 1) $ string defAttr "ModuleHead"

instance Pretty z => EditorMode (z ==> ModuleHead ()) where
  type ParentMode (z ==> ModuleHead ()) = z

  handleKeyEvent (KChar 'q', []) _ = mzero
  handleKeyEvent (KChar 't', []) s =
    return $ Left $ over zipper ascend s
  handleKeyEvent (KChar 'h', []) s =
    Right <$> runEditorMode (over zipper (hRightward . descendHList . descendLens (_Wrapped' . from h4)) s)
  handleKeyEvent e s = do
    liftIO $ print $ "unknown key event type " ++ show e
    return $ Right s

  modeOverlay = do
    height <- use (bounds . _2)
    return $ translateY (height - 1) $ string defAttr "Just ModuleHead"

instance (
      Pretty z,
      Ascend (HZipper (z ==> x) l r (ModuleName ())),
      BuildsOn (HZipper (z ==> x) l r (ModuleName ())) ~ (z ==> x)
    ) =>
      EditorMode (HZipper (z ==> x) l r (ModuleName ()))
    where
  type ParentMode (HZipper (z ==> x) l r (ModuleName ())) = z

  handleKeyEvent (KChar 'q', []) _ = mzero
  handleKeyEvent (KChar 't', []) s =
    return $ Left $ over zipper (ascend . ascend) s
  handleKeyEvent e s = do
    liftIO $ print $ "unknown key event type " ++ show e
    return $ Right s

  modeOverlay = do
    height <- use (bounds . _2)
    return $ translateY (height - 1) $ string defAttr "ModuleName"

type InEditorMode z m = (MMonad z m, EditorMode z, MonadState (State z) m)

nextKeyEvent :: InEditorMode z m => m (Key, [Modifier])
nextKeyEvent = do
  vty <- ask
  e <- liftIO $ nextEvent vty
  case e of
    EvKey k m -> return (k, m)
    EvResize width height -> do
      bounds .= (width, height)
      picture <- generatePicture
      liftIO $ update vty picture
      nextKeyEvent
    _ -> do
      liftIO $ print ("unknown event type " ++ show e)
      nextKeyEvent

generatePicture :: InEditorMode z m => m Picture
generatePicture = do
  height <- use (bounds . _2)
  z <- use zipper
  case prettyPrint z of
    Right src -> do
      image <- modeOverlay
      return $ (generateView (0, 0) height $ lines src) `addToTop` image
    Left err ->
      return $ picForImage $ string defAttr err

handleNextKeyEvent :: (MMonad z m, EditorMode z) => State z -> m (Either (State (ParentMode z)) (State z))
handleNextKeyEvent state =
  flip runStateT state nextKeyEvent >>= uncurry handleKeyEvent

runEditorMode :: (MMonad z m, EditorMode z) => State z -> m (State (ParentMode z))
runEditorMode state = do
  vty <- ask
  state' <-
    flip execStateT state $ do
      pic <- generatePicture
      liftIO $ update vty pic

  state'' <- handleNextKeyEvent state'
  case state'' of
    Left x -> return x
    Right state''' -> runEditorMode state'''

run :: Vty -> State (Root (Module ())) -> IO ()
run vty state = do
  runMaybeT $ flip runReaderT vty $ runEditorMode state
  return ()

loadState :: [String] -> IO ((Int, Int) -> State (Root (Module ())))
loadState [] =
  return $ \bounds -> State bounds (root $ Module () Nothing [] [] []) True
loadState [filename] = do
  src <- readFile filename
  case parseFileContents src of
    ParseOk m -> return $ \bounds -> State bounds (root $ fmap (const ()) m) False
    _ -> undefined
