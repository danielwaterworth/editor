{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module HZipper where

import Data.Proxy

import Control.Lens
import Control.Zipper.Simple

data HList (r :: [*]) where
  HNil  :: HList '[]
  HCons :: e -> HList x -> HList (e ': x)

h2 :: Iso (HList [a, b]) (HList [p, q]) (a, b) (p, q)
h2 =
  iso
    (\(HCons a (HCons b HNil)) -> (a, b))
    (\(a, b) -> HCons a (HCons b HNil))

h3 :: Iso (HList [a, b, c]) (HList [x, y, z]) (a, b, c) (x, y, z)
h3 =
  iso
    (\(HCons a (HCons b (HCons c HNil))) -> (a, b, c))
    (\(x, y, z) -> HCons x (HCons y (HCons z HNil)))

h4 :: Iso (HList [a, b, c, d]) (HList [u, v, w, x]) (a, b, c, d) (u, v, w, x)
h4 =
  iso
    (\(HCons a (HCons b (HCons c (HCons d HNil)))) -> (a, b, c, d))
    (\(u, v, w, x) -> HCons u (HCons v (HCons w (HCons x HNil))))

h5 :: Iso (HList [a, b, c, d, e]) (HList [u, v, w, x, y]) (a, b, c, d, e) (u, v, w, x, y)
h5 =
  iso
    (\(HCons a (HCons b (HCons c (HCons d (HCons e HNil))))) -> (a, b, c, d, e))
    (\(u, v, w, x, y) -> HCons u (HCons v (HCons w (HCons x (HCons y HNil)))))

h6 :: Iso (HList [a, b, c, d, e, f]) (HList [u, v, w, x, y, z]) (a, b, c, d, e, f) (u, v, w, x, y, z)
h6 =
  iso
    (\(HCons a (HCons b (HCons c (HCons d (HCons e (HCons f HNil)))))) -> (a, b, c, d, e, f))
    (\(u, v, w, x, y, z) -> HCons u (HCons v (HCons w (HCons x (HCons y (HCons z HNil))))))

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

instance Rooted z => Rooted (HZipper z '[] r x) where
  type RootedAt (HZipper z '[] r x) = RootedAt z

instance
    (
      Rooted z,
      LeftMost (HZipper z ls (x : r) l),
      Ascend (LeftMostT (HZipper z ls (x : r) l)),
      BuildsOn (LeftMostT (HZipper z ls (x : r) l)) ~ z
    ) =>
      Rooted (HZipper z (l ': ls) r x) where
  type RootedAt (HZipper z (l ': ls) r x) = RootedAt z

instance Focused (HZipper z l r x) where
  type FocusedAt (HZipper z l r x) = x

  focus f (HZipper g l r x) = HZipper g l r <$> f x

instance Ascend (HZipper z '[] r x) where
  type BuildsOn (HZipper z '[] r x) = z

  ascend (HZipper f _ r x) = f $ HCons x r

instance
    (
      LeftMost (HZipper z ls (x ': r) l),
      Ascend (LeftMostT (HZipper z ls (x ': r) l)),
      BuildsOn (LeftMostT (HZipper z ls (x ': r) l)) ~ z
    ) =>
      Ascend (HZipper z (l ': ls) r x) where
  type BuildsOn (HZipper z (l ': ls) r x) = z

  ascend = ascend . leftmost
