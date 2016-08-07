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

import Data.List (intersperse)
import Data.Constraint

import Control.Applicative
import Control.Monad
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

class Pretty a where
  pretty :: Printer m => a -> m ()

instance {-# OVERLAPPABLE #-} Pretty a where
  pretty _ = mzero

instance Pretty () where
  pretty = return

instance Pretty (Module ()) where
  pretty = prettyPrism _Module'

instance Pretty (C_Module ()) where
  pretty = mapPrettySepByM newline . view _Wrapped
