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

import Control.Lens (makeWrapped, Prism', _Unwrapped)
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
          Just name' <- TH.lookupTypeName $ "C_" ++ TH.nameBase name
          makeWrapped name')
    else
      return []))

$(concat <$>
  forM types (\tName -> do
    TH.TyConI (TH.DataD _ _ binders Nothing constructors deriv) <- TH.reify tName
    if length constructors >= 2 then
      concat <$>
        forM constructors (\(TH.NormalC cName tys) -> do
          Just ty <- TH.lookupTypeName $ "C_" ++ TH.nameBase cName
          Just prismTy <- TH.lookupTypeName "Prism'"
          Just compose <- TH.lookupValueName "."
          Just unwrapped <- TH.lookupValueName "_Unwrapped"
          Just prism <- TH.lookupValueName $ "_" ++ TH.nameBase cName

          let name' = TH.mkName $ "_" ++ TH.nameBase cName ++ "'"
          vars <- replicateM (length binders) $ TH.newName "v"
          let f x = foldl' TH.AppT x $ map TH.VarT vars
          let baseTy = f $ TH.ConT tName
          let cTy = f $ TH.ConT ty
          let binding = TH.SigD name' $ TH.ConT prismTy `TH.AppT` baseTy `TH.AppT` cTy
          let exp = TH.ParensE (TH.VarE compose) `TH.AppE` TH.VarE prism `TH.AppE` TH.VarE unwrapped
          let v = TH.ValD (TH.VarP name') (TH.NormalB exp) []
          return [binding, v])
    else
      return []))
