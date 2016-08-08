{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module TaggedZipper where

import Data.Proxy

import Control.Lens
import Control.Lens.TH
import Control.Zipper.Simple

data TaggedZipper z t =
  TaggedZipper { untag :: z }
makeWrapped ''TaggedZipper

tag :: Proxy t -> z -> TaggedZipper z t
tag _ = TaggedZipper

instance Ascend z => Ascend (TaggedZipper z t) where
  type BuildsOn (TaggedZipper z t) = BuildsOn z

  ascend = ascend . untag

instance Rooted z => Rooted (TaggedZipper z t) where
  type RootedAt (TaggedZipper z t) = RootedAt z

  rezip = rezip . untag

instance Focused z => Focused (TaggedZipper z t) where
  type FocusedAt (TaggedZipper z t) = FocusedAt z

  focus = (_Unwrapped :: Iso' z (TaggedZipper z t)) . focus
