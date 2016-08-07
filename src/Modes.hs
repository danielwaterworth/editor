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

import Control.Applicative

import Control.Monad (void)
import Control.Monad.Reader (MonadReader, ask, runReaderT)
import Control.Monad.State (execStateT, runStateT, StateT, MonadState, get, put)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Maybe (runMaybeT, MaybeT)
import Control.Monad (MonadPlus, mzero)
import Control.Zipper.Simple
import Control.Monad.Trans.Either

import Graphics.Vty
import Language.Haskell.Exts

import Pretty
import View
import Prisms

data State h =
  State {
    _bounds :: (Int, Int),
    _moduleStateFilename :: [Char],
    _zipper :: h,
    _moduleStateDirty :: Bool
  }
makeLenses ''State

tug :: (a -> Maybe a) -> a -> a
tug f x =
  case f x of
    Nothing -> x
    Just x' -> x'

type MMonad z m = (
    Rooted z,
    RootedAt z ~ Module SrcSpanInfo,
    MonadIO m,
    MonadPlus m,
    MonadReader Vty m
  )

class EditorMode z where
  handleKeyEvent :: MMonad z m => (Key, [Modifier]) -> State z -> m (Either (State (BuildsOn z)) (State z))

  modeOverlay :: (MMonad z m, MonadState (State z) m) => m Image

instance EditorMode (Root (Module SrcSpanInfo)) where
  handleKeyEvent (KChar 'q', []) _ = mzero
  handleKeyEvent (KChar 'h', []) s =
    Right <$>
      case descendPrism _Module' $ view zipper s of
        Nothing -> do
          liftIO $ print "wrong kind of module"
          return s
        Just z' ->
          runEditorMode $ set zipper z' s
  handleKeyEvent e s = do
    liftIO $ print $ "unknown key event type " ++ show e
    return $ Right s

  modeOverlay = return $ string defAttr "MODULE"

instance EditorMode (z ==> C_Module SrcSpanInfo) where
  handleKeyEvent (KChar 'q', []) _ = mzero
  handleKeyEvent (KChar 'd', []) s =
    Right <$> runEditorMode (over zipper (descendLens (_Wrapped . _5)) s)
  handleKeyEvent e s = do
    liftIO $ print $ "unknown key event type " ++ show e
    return $ Right s

  modeOverlay = return $ string defAttr "HASKELL MODULE"

instance EditorMode (z ==> [Decl SrcSpanInfo]) where
  handleKeyEvent (KChar 'q', []) _ = mzero
  handleKeyEvent (KChar 'f', []) s =
    Right <$>
      case descendList $ view zipper s of
        Nothing -> do
          liftIO $ print "can't descend into empty list"
          return s
        Just z' ->
          runEditorMode $ set zipper z' s
  handleKeyEvent e s = do
    liftIO $ print $ "unknown key event type " ++ show e
    return $ Right s

  modeOverlay = return $ string defAttr "DECL LIST"

instance EditorMode (z =*=> Decl SrcSpanInfo) where
  handleKeyEvent (KChar 'q', []) _ = mzero
  handleKeyEvent (KChar 'x', []) s =
    case deleteFocus $ view zipper s of
      Left z' -> return $ Left $ set zipper z' s
      Right z' -> return $ Right $ set zipper z' s
  handleKeyEvent (KChar 'n', []) s =
    return $ Right $ over zipper (tug rightward) s
  handleKeyEvent (KChar 'p', []) s =
    return $ Right $ over zipper (tug leftward) s
  handleKeyEvent e s = do
    liftIO $ print $ "unknown key event type " ++ show e
    return $ Right s

  modeOverlay = return $ string defAttr "DECL"

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
  let mod = rezip z
  let src = prettyPrint mod

  image <- modeOverlay
  return $ (generateView (0, 0) height $ lines src) `addToTop` image

handleNextKeyEvent :: (MMonad z m, EditorMode z) => StateT (EitherT (State (BuildsOn z))) m ()
handleNextKeyEvent = do
  nextKeyEvent >>= juggle handleKeyEvent

juggle :: Monad m => (a -> s -> m (Either e s)) -> a -> StateT (EitherT e) m s
juggle = fmap modifyT . (fmap . fmap) EitherT

modifyT :: Monad m => (s -> m s) -> StateT s m ()
modifyT f = StateT $ \s -> (,) () <$> f s

runEditorMode :: (MMonad z m, EditorMode z) => State z -> EitherT (State (BuildsOn z)) m Void
runEditorMode = evalStateT $ forever $ do
  vty <- ask
  pic <- generatePicture
  liftIO $ update vty pic
  handleNextKeyEvent

run :: Vty -> State (Root (Module SrcSpanInfo)) -> IO ()
run vty = void . runMaybeT . flip runReaderT vty . eitherT return absurd . runEditorMode

loadState :: [String] -> IO ((Int, Int) -> State (Root (Module SrcSpanInfo)))
loadState [filename] = do
  src <- readFile filename
  case parseFileContents src of
    ParseOk m -> return $ \bounds -> State bounds filename (root m) False
    _ -> undefined
