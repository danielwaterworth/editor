{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
module UnfocusedListZipper where

import Data.Typeable

import Control.Lens
import Control.Zipper.Simple

data z =%=> a =
  UMany ([a] -> z) [a] [a]
    deriving (Typeable)

instance Ascend (z =%=> a) where
  type BuildsOn (z =%=> a) = z

  ascend (UMany f l r) = f $ reverse l ++ r

instance Rooted z => Rooted (z =%=> a) where
  type RootedAt (z =%=> a) = RootedAt z

descendUList :: (Focused z, FocusedAt z ~ [x]) => z -> z =%=> x
descendUList z = UMany (flip (set focus) z) [] $ view focus z

insert :: x -> z =%=> x -> z =%=> x
insert x (UMany f l r) = UMany f (x : l) r

backspace :: z =%=> x -> z =%=> x
backspace (UMany f l r) = UMany f (drop 1 l) r
