{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module TaggedZipper where

import Data.Proxy

import Control.Lens
import Control.Lens.TH
import Control.Zipper.Simple

data TaggedZipper t z =
  TaggedZipper { untag :: z }
makeWrapped ''TaggedZipper

tag :: Proxy t -> z -> TaggedZipper t z
tag _ = TaggedZipper

instance Ascend z => Ascend (TaggedZipper t z) where
  type BuildsOn (TaggedZipper t z) = BuildsOn z

  ascend = ascend . untag

instance Rooted z => Rooted (TaggedZipper t z) where
  type RootedAt (TaggedZipper t z) = RootedAt z

  rezip = rezip . untag

instance Focused z => Focused (TaggedZipper t z) where
  type FocusedAt (TaggedZipper t z) = FocusedAt z

  focus = (_Unwrapped :: Iso' z (TaggedZipper t z)) . focus
