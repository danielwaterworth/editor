{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module HZipper where

import Control.Lens
import Control.Zipper.Simple

data HList (r :: [*]) where
  HNil  :: HList '[]
  HCons :: e -> HList x -> HList (e ': x)

type family A x where
  A (HZipper z '[] r x) = HList (x ': r)

data HZipper (z :: *) (l :: [*]) (r :: [*]) (x :: *) =
  HZipper (A (LeftMostT (HZipper z l r x)) -> z) (HList l) (HList r) x

descendHList :: (Focused z, FocusedAt z ~ HList (r ': rs)) => z -> HZipper z '[] rs r
descendHList z =
  case view focus z of
    HCons r rs -> HZipper (flip (set focus) z) HNil rs r

hRightward :: HZipper z l (r ': rs) x -> HZipper z (x ': l) rs r
hRightward (HZipper f l (HCons r rs) x) = HZipper f (HCons x l) rs r

hLeftward :: HZipper z (l ': ls) r x -> HZipper z ls (x ': r) l
hLeftward (HZipper f (HCons l ls) r x) = HZipper f ls (HCons x r) l

class LeftMost z where
  type LeftMostT z

  leftmost :: z -> LeftMostT z

instance LeftMost (HZipper z '[] r x) where
  type LeftMostT (HZipper z '[] r x) = HZipper z '[] r x

  leftmost = id

instance LeftMost (HZipper z ls (x ': r) l) => LeftMost (HZipper z (l ': ls) r x) where
  type LeftMostT (HZipper z (l ': ls) r x) = LeftMostT (HZipper z ls (x ': r) l)

  leftmost = leftmost . hLeftward

instance Focused (HZipper z l r x) where
  type FocusedAt (HZipper z l r x) = x

  focus f (HZipper g l r x) = HZipper g l r <$> f x

instance Ascend (HZipper z '[] r x) where
  type BuildsOn (HZipper z '[] r x) = z

  ascend (HZipper f _ r x) = f $ HCons x r

instance
    (LeftMost (HZipper z ls (x ': r) l), Ascend (LeftMostT (HZipper z ls (x ': r) l))) =>
    Ascend (HZipper z (l ': ls) r x) where
  type BuildsOn (HZipper z (l ': ls) r x) = BuildsOn (LeftMostT (HZipper z ls (x ': r) l))

  ascend = ascend . leftmost
