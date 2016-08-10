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

fromRight :: Either a b -> b
fromRight (Right x) = x
fromRight _ = error "fromRight"

tug :: (a -> Maybe a) -> a -> a
tug f x =
  case f x of
    Nothing -> x
    Just x' -> x'

type MMonad m = (
    MonadIO m,
    MonadPlus m,
    MonadReader Vty m
  )

type ParentState mode = State (ParentMode mode)

class EditorMode mode where
  type ParentMode mode
  type Zipper mode

  handleKeyEvent :: MMonad m => Proxy mode -> (Key, [Modifier]) -> State (Zipper mode) -> m (Either (ParentState mode) (State (Zipper mode)))

  modeOverlay :: (MMonad m, MonadState (State (Zipper mode)) m) => Proxy mode -> m Image

instance EditorMode (Root (Module ())) where
  type ParentMode (Root (Module ())) = Void
  type Zipper (Root (Module ())) = Root (Module ())

  handleKeyEvent _ (KChar 'q', []) _ = mzero
  handleKeyEvent _ (KChar 'h', []) s =
    Right <$>
      case descendPrism _Module' $ view zipper s of
        Just z ->
          runEditorMode (Proxy :: Proxy (Root (Module ()) ==> C_Module ())) $ set zipper z s
        Nothing -> do
          liftIO $ print "Wrong type of module"
          return s
  handleKeyEvent _ e s = do
    liftIO $ print $ "unknown key event type " ++ show e
    return $ Right s

  modeOverlay _ = do
    height <- use (bounds . _2)
    return $ translateY (height - 1) $ string defAttr "Module"

instance Pretty z => EditorMode (z ==> C_Module ()) where
  type ParentMode (z ==> C_Module ()) = z
  type Zipper (z ==> C_Module ()) = z ==> C_Module ()

  handleKeyEvent _ (KChar 'q', []) _ = mzero
  handleKeyEvent _ (KChar 't', []) s =
    return $ Left $ over zipper ascend s
  handleKeyEvent _ (KChar 'h', []) s =
    Right <$>
      runEditorMode
        (Proxy ::
          Proxy
            (HZipper
              (z ==> C_Module () ==> HList '[(), Maybe (ModuleHead ()), [ModulePragma ()], [ImportDecl ()], [Decl ()]])
              '[()]
              '[[ModulePragma ()], [ImportDecl ()], [Decl ()]]
              (Maybe (ModuleHead ()))))
        (over zipper (hRightward . descendHList . descendLens (_Wrapped . from h5)) s)
  handleKeyEvent _ e s = do
    liftIO $ print $ "unknown key event type " ++ show e
    return $ Right s

  modeOverlay _ = do
    height <- use (bounds . _2)
    return $ translateY (height - 1) $ string defAttr "Haskell Module"

instance (
      Pretty z,
      Ascend (HZipper (z ==> x) l ([ModulePragma ()] ': r) (Maybe (ModuleHead ()))),
      Ascend (HZipper (z ==> x) (Maybe (ModuleHead ()) ': l) r [ModulePragma ()]),
      BuildsOn (HZipper (z ==> x) l ([ModulePragma ()] ': r) (Maybe (ModuleHead ()))) ~ (z ==> x),
      BuildsOn (HZipper (z ==> x) (Maybe (ModuleHead ()) ': l) r [ModulePragma ()]) ~ (z ==> x)
    ) =>
      EditorMode (HZipper (z ==> x) l ([ModulePragma ()] ': r) (Maybe (ModuleHead ())))
    where
  type ParentMode (HZipper (z ==> x) l ([ModulePragma ()] ': r) (Maybe (ModuleHead ()))) = z
  type Zipper (HZipper (z ==> x) l ([ModulePragma ()] ': r) (Maybe (ModuleHead ()))) = HZipper (z ==> x) l ([ModulePragma ()] ': r) (Maybe (ModuleHead ()))

  handleKeyEvent _ (KChar 'q', []) _ = mzero
  handleKeyEvent _ (KChar 't', []) s =
    return $ Left $ over zipper (ascend . ascend) s
  handleKeyEvent _ (KChar 'h', []) s =
    Right <$>
      case descendPrism _Just $ view zipper s of
        Just z ->
          runEditorMode
            (Proxy ::
              Proxy
                (HZipper (z ==> x) l ([ModulePragma ()] ': r) (Maybe (ModuleHead ())) ==>
                  ModuleHead ()))
            (set zipper z s)
        Nothing -> do
          liftIO $ print "Can't descend into Nothing"
          return s
  handleKeyEvent _ (KChar 's', []) s = do
    Left <$>
      runEditorMode
        (Proxy :: Proxy (HZipper (z ==> x) (Maybe (ModuleHead ()) ': l) r [ModulePragma ()]))
        (over zipper hRightward s)
  handleKeyEvent _ (KChar 'X', []) s =
    return $ Right $ set (zipper . focus) Nothing s
  handleKeyEvent _ (KChar 'M', []) s =
    return $ Right $ set (zipper . focus) (Just (ModuleHead () (ModuleName () "") Nothing Nothing)) s
  handleKeyEvent _ e s = do
    liftIO $ print $ "unknown key event type " ++ show e
    return $ Right s

  modeOverlay _ = do
    height <- use (bounds . _2)
    return $ translateY (height - 1) $ string defAttr "ModuleHead"

instance (
      Pretty z,
      Ascend (HZipper (z ==> x) l ([ModulePragma ()] ': r) (Maybe (ModuleHead ()))),
      Ascend (HZipper (z ==> x) (Maybe (ModuleHead ()) ': l) r [ModulePragma ()]),
      BuildsOn (HZipper (z ==> x) l ([ModulePragma ()] ': r) (Maybe (ModuleHead ()))) ~ (z ==> x),
      BuildsOn (HZipper (z ==> x) (Maybe (ModuleHead ()) ': l) r [ModulePragma ()]) ~ (z ==> x)
    ) =>
      EditorMode (HZipper (z ==> x) (Maybe (ModuleHead ()) ': l) r [ModulePragma ()])
    where
  type ParentMode (HZipper (z ==> x) (Maybe (ModuleHead ()) ': l) r [ModulePragma ()]) = z
  type Zipper (HZipper (z ==> x) (Maybe (ModuleHead ()) ': l) r [ModulePragma ()]) = (HZipper (z ==> x) (Maybe (ModuleHead ()) ': l) r [ModulePragma ()])

  handleKeyEvent _ (KChar 'q', []) _ = mzero
  handleKeyEvent _ (KChar 't', []) s =
    return $ Left $ over zipper (ascend . ascend) s
  handleKeyEvent _ (KChar 'n', []) s =
    Left <$>
      runEditorMode
        (Proxy :: Proxy (HZipper (z ==> x) l ([ModulePragma ()] ': r) (Maybe (ModuleHead ()))))
        (over zipper hLeftward s)
  handleKeyEvent _ e s = do
    liftIO $ print $ "unknown key event type " ++ show e
    return $ Right s

  modeOverlay _ = do
    height <- use (bounds . _2)
    return $ translateY (height - 1) $ string defAttr "ModulePragmas"

instance Pretty z => EditorMode (z ==> ModuleHead ()) where
  type ParentMode (z ==> ModuleHead ()) = z
  type Zipper (z ==> ModuleHead ()) = (z ==> ModuleHead ())

  handleKeyEvent _ (KChar 'q', []) _ = mzero
  handleKeyEvent _ (KChar 't', []) s =
    return $ Left $ over zipper ascend s
  handleKeyEvent _ (KChar 'h', []) s =
    Right <$>
      runEditorMode
        (Proxy ::
          Proxy
            (HZipper
              (z ==> ModuleHead () ==> HList _)
              _
              _
              (ModuleName ())))
        (over zipper (hRightward . descendHList . descendLens (_Wrapped' . from h4)) s)
  handleKeyEvent _ e s = do
    liftIO $ print $ "unknown key event type " ++ show e
    return $ Right s

  modeOverlay _ = do
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
  type Zipper (HZipper (z ==> x) l r (ModuleName ())) = (HZipper (z ==> x) l r (ModuleName ()))

  handleKeyEvent _ (KChar 'q', []) _ = mzero
  handleKeyEvent _ (KChar 't', []) s =
    return $ Left $ over zipper (ascend . ascend) s
  handleKeyEvent _ (KChar 'h', []) s =
    Right <$>
      runEditorMode
        (Proxy ::
          Proxy
            (HZipper
              (HZipper (z ==> x) l r (ModuleName ()) ==> HList '[(), String])
              '[()]
              '[]
              String))
        (over zipper (hRightward . descendHList . descendLens (_Wrapped' . from h2)) s)
  handleKeyEvent _ e s = do
    liftIO $ print $ "unknown key event type " ++ show e
    return $ Right s

  modeOverlay _ = do
    height <- use (bounds . _2)
    return $ translateY (height - 1) $ string defAttr "ModuleName"

instance (
      Pretty z,
      Ascend (HZipper (z ==> x) l r String),
      BuildsOn (HZipper (z ==> x) l r String) ~ (z ==> x)
    ) =>
      EditorMode (HZipper (z ==> x) l r String)
    where
  type ParentMode (HZipper (z ==> x) l r String) = z
  type Zipper (HZipper (z ==> x) l r String) = (HZipper (z ==> x) l r String)

  handleKeyEvent _ (KChar 'q', []) _ = mzero
  handleKeyEvent _ (KChar 't', []) s =
    return $ Left $ over zipper (ascend . ascend) s
  handleKeyEvent _ (KChar 'E', []) s =
    Right <$>
      runEditorMode
        (Proxy :: Proxy (HZipper (z ==> x) l r String =%=> Char))
        (over zipper descendUList s)
  handleKeyEvent _ e s = do
    liftIO $ print $ "unknown key event type " ++ show e
    return $ Right s

  modeOverlay _ = do
    height <- use (bounds . _2)
    return $ translateY (height - 1) $ string defAttr "String"

instance Pretty z => EditorMode (z =%=> Char) where
  type ParentMode (z =%=> Char) = z
  type Zipper (z =%=> Char) = (z =%=> Char)

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

nextKeyEvent :: (Pretty (Zipper z), InEditorMode z m) => Proxy z -> m (Key, [Modifier])
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

generatePicture :: (Pretty (Zipper z), InEditorMode z m) => Proxy z -> m Picture
generatePicture proxy = do
  height <- use (bounds . _2)
  z <- use zipper
  case prettyPrint z of
    Right src -> do
      image <- modeOverlay proxy
      return $ (generateView (0, 0) height $ lines src) `addToTop` image
    Left err ->
      return $ picForImage $ string defAttr err

handleNextKeyEvent :: (Pretty (Zipper z), MMonad m, EditorMode z) => Proxy z -> State (Zipper z) -> m (Either (ParentState z) (State (Zipper z)))
handleNextKeyEvent proxy state =
  flip runStateT state (nextKeyEvent proxy) >>= uncurry (handleKeyEvent proxy)

runEditorMode :: (Pretty (Zipper z), MMonad m, EditorMode z) => Proxy z -> State (Zipper z) -> m (ParentState z)
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
  runMaybeT $ flip runReaderT vty $ runEditorMode (Proxy :: Proxy (Root (Module ()))) state
  return ()

loadState :: [String] -> IO ((Int, Int) -> State (Root (Module ())))
loadState [] =
  return $ \bounds -> State bounds (root $ Module () Nothing [] [] []) True
loadState [filename] = do
  src <- readFile filename
  case parseFileContents src of
    ParseOk m -> return $ \bounds -> State bounds (root $ fmap (const ()) m) False
    _ -> undefined
