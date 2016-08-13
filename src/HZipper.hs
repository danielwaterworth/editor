{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
module HZipper where

import Control.Lens
import Control.Zipper.Simple

data HList (l :: [*]) :: * where
  HNil :: HList '[]
  HCons :: l -> HList ls -> HList (l ': ls)

h2 :: Iso (a, b) (p, q) (HList [a, b]) (HList [p, q])
h2 =
  iso
    (\(a, b) -> HCons a (HCons b HNil))
    (\(HCons a (HCons b HNil)) -> (a, b))

h3 :: Iso (a, b, c) (x, y, z) (HList [a, b, c]) (HList [x, y, z])
h3 =
  iso
    (\(x, y, z) -> HCons x (HCons y (HCons z HNil)))
    (\(HCons a (HCons b (HCons c HNil))) -> (a, b, c))

h4 :: Iso (a, b, c, d) (u, v, w, x) (HList [a, b, c, d]) (HList [u, v, w, x])
h4 =
  iso
    (\(u, v, w, x) -> HCons u (HCons v (HCons w (HCons x HNil))))
    (\(HCons a (HCons b (HCons c (HCons d HNil)))) -> (a, b, c, d))

h5 :: Iso (a, b, c, d, e) (u, v, w, x, y) (HList [a, b, c, d, e]) (HList [u, v, w, x, y])
h5 =
  iso
    (\(u, v, w, x, y) -> HCons u (HCons v (HCons w (HCons x (HCons y HNil)))))
    (\(HCons a (HCons b (HCons c (HCons d (HCons e HNil))))) -> (a, b, c, d, e))

h6 :: Iso (a, b, c, d, e, f) (u, v, w, x, y, z) (HList [a, b, c, d, e, f]) (HList [u, v, w, x, y, z])
h6 =
  iso
    (\(u, v, w, x, y, z) -> HCons u (HCons v (HCons w (HCons x (HCons y (HCons z HNil))))))
    (\(HCons a (HCons b (HCons c (HCons d (HCons e (HCons f HNil)))))) -> (a, b, c, d, e, f))

type family Zipped l r x where
  Zipped '[] r x = x ': r
  Zipped (l ': ls) r x = Zipped ls (x ': r) l

data HZipper z l r x =
  HZipper (HList (Zipped l r x) -> z) (HList l) (HList r) x

hLeftward :: HZipper z (l ': ls) r x -> HZipper z ls (x ': r) l
hLeftward (HZipper f (HCons l ls) r x) = HZipper f ls (HCons x r) l

hRightward :: HZipper z l (r ': rs) x -> HZipper z (x ': l) rs r
hRightward (HZipper f l (HCons r rs) x) = HZipper f (HCons x l) rs r

descendHList :: (Focused z, FocusedAt z ~ HList (r ': rs)) => z -> HZipper z '[] rs r
descendHList z =
  case view focus z of
    HCons r rs -> HZipper (flip (set focus) z) HNil rs r

instance Ascend (HZipper z l r x) where
  type BuildsOn (HZipper z l r x) = z

  ascend (HZipper f HNil r x) = f $ HCons x r
  ascend (HZipper f (HCons l ls) r x) = ascend (HZipper f ls (HCons x r) l)

instance Rooted z => Rooted (HZipper z l r x) where
  type RootedAt (HZipper z l r x) = RootedAt z

instance Focused (HZipper z l r x) where
  type FocusedAt (HZipper z l r x) = x

  focus g (HZipper f l r x) = HZipper f l r <$> g x
