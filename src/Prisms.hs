{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module Prisms where

import Language.Haskell.Exts

import Data.List (foldl')

import Control.Monad
import Control.Applicative

import Control.Lens hiding (Context)
import Control.Lens.TH
import Data.Generics.SYB.WithClass.Derive

import qualified Language.Haskell.TH as TH

import TypeList

concat <$> mapM makePrisms types
deriveData types

$(concat <$>
  forM types (\name -> do
    TH.TyConI (TH.DataD _ _ binders Nothing constructors deriv) <- TH.reify name
    if length constructors >= 2 then
      forM constructors (\(TH.NormalC name tys) -> do
        let name' = TH.mkName $ "C_" ++ TH.nameBase name
        let ty = foldl' TH.AppT (TH.TupleT $ length tys) $ map snd tys
        let unbanged = TH.Bang TH.NoSourceUnpackedness TH.NoSourceStrictness
        return $ TH.NewtypeD [] name' binders Nothing (TH.NormalC name' [(unbanged, ty)]) deriv)
     else
      return []))

$(concat <$>
  forM types (\name -> do
    TH.TyConI (TH.DataD _ _ binders Nothing constructors deriv) <- TH.reify name
    if length constructors >= 2 then
      concat <$>
        forM constructors (\(TH.NormalC name tys) -> do
          let name' = TH.mkName $ "C_" ++ TH.nameBase name
          makeWrapped name')
    else
      return []))

_Module' :: Prism' (Module l) (C_Module l)
_Module' = _Module . _Unwrapped

_IVar' :: Prism' (ImportSpec l) (C_IVar l)
_IVar' = _IVar . _Unwrapped

_IAbs' :: Prism' (ImportSpec l) (C_IAbs l)
_IAbs' = _IAbs . _Unwrapped

_IThingAll' :: Prism' (ImportSpec l) (C_IThingAll l)
_IThingAll' = _IThingAll . _Unwrapped

_IThingWith' :: Prism' (ImportSpec l) (C_IThingWith l)
_IThingWith' = _IThingWith . _Unwrapped

_Ident' :: Prism' (Name l) (C_Ident l)
_Ident' = _Ident . _Unwrapped

_Symbol' :: Prism' (Name l) (C_Symbol l)
_Symbol' = _Symbol . _Unwrapped
