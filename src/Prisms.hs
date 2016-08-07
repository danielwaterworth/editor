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

(\names -> do
  prisms <- concat <$> mapM makePrisms names
  dataInstances <- deriveData names

  decls <-
    concat <$>
      forM names (\name -> do
        TH.TyConI (TH.DataD _ _ binders Nothing constructors deriv) <- TH.reify name
        if length constructors >= 2 then
          forM constructors (\(TH.NormalC name tys) -> do
            let name' = TH.mkName $ "C_" ++ TH.nameBase name
            let ty = foldl' TH.AppT (TH.TupleT $ length tys) $ map snd tys
            let unbanged = TH.Bang TH.NoSourceUnpackedness TH.NoSourceStrictness
            return $ TH.NewtypeD [] name' binders Nothing (TH.NormalC name' [(unbanged, ty)]) deriv)
         else
          return [])

  return $ prisms ++ dataInstances ++ decls) [
    ''Module,
    ''ModuleHead,
    ''WarningText,
    ''ExportSpecList,
    ''ExportSpec,
    ''EWildcard,
    ''ImportDecl,
    ''ImportSpecList,
    ''ImportSpec,
    ''Assoc,
    ''Namespace,
    ''Decl,
    ''DeclHead,
    ''InstRule,
    ''InstHead,
    ''Binds,
    ''IPBind,
    ''PatternSynDirection,
    ''InjectivityInfo,
    ''ResultSig,
    ''ClassDecl,
    ''InstDecl,
    ''Deriving,
    ''DataOrNew,
    ''ConDecl,
    ''FieldDecl,
    ''QualConDecl,
    ''GadtDecl,
    ''BangType,
    ''Unpackedness,
    ''Match,
    ''Rhs,
    ''GuardedRhs,
    ''Context,
    ''FunDep,
    ''Asst,
    ''Type,
    ''Boxed,
    ''Kind,
    ''TyVarBind,
    ''Promoted,
    ''TypeEqn,
    ''Exp,
    ''Stmt,
    ''QualStmt,
    ''FieldUpdate,
    ''Alt,
    ''XAttr,
    ''Pat,
    ''PatField,
    ''PXAttr,
    ''RPat,
    ''RPatOp,
    ''Literal,
    ''Sign,
    ''ModuleName,
    ''QName,
    ''Name,
    ''QOp,
    ''Op,
    ''SpecialCon,
    ''CName,
    ''IPName,
    ''XName,
    ''Role,
    ''Bracket,
    ''Splice,
    ''Safety,
    ''CallConv,
    ''ModulePragma,
    ''Tool,
    ''Overlap,
    ''Rule,
    ''RuleVar,
    ''Activation,
    ''Annotation,
    ''BooleanFormula
  ]

makeWrapped ''C_Module

_Module' :: Prism' (Module l) (C_Module l)
_Module' = _Module . _Unwrapped
