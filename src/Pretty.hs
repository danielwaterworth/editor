{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Pretty where

import Language.Haskell.Exts hiding (Pretty, pretty)

import Data.Maybe
import Data.List (intersperse)
import Data.Constraint
import Data.Foldable

import Control.Applicative
import Control.Monad (MonadPlus, mzero, when, guard)
import Control.Lens
import Control.Zipper.Simple
import Data.Generics.SYB.WithClass.Basics
import Data.Generics.SYB.WithClass.Instances ()

import Prisms

class (Applicative m, Monad m, MonadPlus m) => Printer m where
  s :: String -> m ()
  highlight :: m a -> m a
  newline :: m ()

pull :: MonadPlus m => Maybe a -> m a
pull = maybe mzero return

prettyPrism :: (Printer m, Pretty a) => Prism' s a -> s -> m ()
prettyPrism p x =
  pull (preview p x) >>= pretty

newtype DPretty a = DPretty (Dict (Pretty a))

instance Pretty a => Sat (DPretty a) where
  dict = DPretty Dict

prettyD :: forall a m. (Data DPretty a, Printer m) => a -> m ()
prettyD x =
  case (dict :: DPretty a) of
    DPretty Dict -> pretty x

mapPrettySepByM :: (Data DPretty a, Printer m) => m () -> a -> m ()
mapPrettySepByM sep =
  sequence_ . intersperse sep . gmapQ (undefined :: Proxy DPretty) prettyD

newlineTwice :: Printer m => m ()
newlineTwice = newline >> newline

parens :: Printer m => m a -> m a
parens m = do
  s "("
  x <- m
  s ")"
  return x

class Pretty a where
  pretty :: Printer m => a -> m ()

instance {-# OVERLAPPABLE #-} Pretty a where
  pretty _ = mzero

instance Pretty () where
  pretty = return

instance Pretty (Module ()) where
  pretty = prettyPrism _Module'

instance Pretty (C_Module ()) where
  pretty (C_Module (_, head, pragmas, imports, decls)) =
    sequence_ $
      intersperse newlineTwice $
        map (sequence_ . intersperse newline) $
          filter (not . null) $
            [map pretty (maybeToList head), map pretty pragmas, map pretty imports, map pretty decls]

instance Pretty (ModuleHead ()) where
  pretty (ModuleHead _ name Nothing Nothing) = do
    s "module "
    pretty name
    s " where"
  pretty _ = mzero

instance Pretty (ModuleName ()) where
  pretty (ModuleName _ x) = s x

instance Pretty (ImportDecl ()) where
  pretty (ImportDecl () mod qualified False False Nothing as spec) = do
    s "import "
    when qualified $ s "qualified "
    pretty mod
    forM_ as $ \name -> do
      s " as "
      pretty name
    pretty spec
  pretty _ = mzero

instance Pretty (Maybe (ImportSpecList ())) where
  pretty x =
    forM_ x $ \(ImportSpecList _ b l) -> do
      s " "
      when b $ s "hiding "
      parens $
        sequence_ $
          intersperse (s ", ") $
            map pretty l

instance Pretty (ImportSpec ()) where
  pretty x =
    msum [
      prettyPrism _IVar' x,
      prettyPrism _IAbs' x,
      prettyPrism _IThingAll' x,
      prettyPrism _IThingWith' x
    ]

instance Pretty (C_IVar ()) where
  pretty (C_IVar ((), n)) = pretty n

instance Pretty (Name ()) where
  pretty x =
    msum [
      prettyPrism _Ident' x,
      prettyPrism _Symbol' x
    ]

instance Pretty (C_Ident ()) where
  pretty (C_Ident ((), name)) = s name
