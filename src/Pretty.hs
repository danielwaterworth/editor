{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Pretty where

import Language.Haskell.Exts hiding (Pretty, pretty)
import Language.Haskell.Exts.Prisms

import Data.Typeable (Typeable, typeOf)
import Data.Maybe
import Data.List (intersperse)
import Data.Foldable

import Control.Applicative
import Control.Monad (when, guard)
import Control.Monad.Except (MonadError, throwError)
import Control.Lens
import Control.Zipper.Simple

import HZipper
import UnfocusedListZipper

class (Applicative m, Monad m, MonadError String m) => Printer m where
  s :: String -> m ()
  highlight :: m a -> m a
  newline :: m ()

prettyPrism :: (Printer m, Pretty a) => Prism' s a -> s -> m Bool
prettyPrism p x = do
  case preview p x of
    Nothing -> return False
    Just v' -> pretty v' >> return True

attemptAll :: Monad m => [m Bool] -> m ()
attemptAll [] = return ()
attemptAll (x : xs) = do
  v <- x
  if v then
    return ()
   else
    attemptAll xs

newlineTwice :: Printer m => m ()
newlineTwice = newline >> newline

parens :: Printer m => m a -> m a
parens m = do
  s "("
  x <- m
  s ")"
  return x

class Pretty a where
  pretty :: Printer m => a -> m ()

instance {-# OVERLAPPABLE #-} Typeable a => Pretty a where
  pretty x = throwError $ "Not implemented: " ++ show (typeOf x)

instance Pretty () where
  pretty = return

instance Pretty a => Pretty (Root a) where
  pretty = pretty . rezip

instance (Pretty z) => Pretty (z ==> a) where
  pretty = pretty . ascend

instance (Ascend (HZipper z l r x), Pretty (BuildsOn (HZipper z l r x))) => Pretty (HZipper z l r x) where
  pretty = pretty . ascend

instance (Pretty z) => Pretty (z =%=> a) where
  pretty = pretty . ascend

instance Pretty (Module ()) where
  pretty x =
    attemptAll [
      prettyPrism _Module' x,
      prettyPrism _XmlPage' x,
      prettyPrism _XmlHybrid' x
    ]

instance Pretty (C_Module ()) where
  pretty (C_Module (_, head, pragmas, imports, decls)) =
    sequence_ $
      intersperse newlineTwice $
        map (sequence_ . intersperse newline) $
          filter (not . null) $
            [map pretty (maybeToList head), map pretty pragmas, map pretty imports, map pretty decls]

instance Pretty (ModuleHead ()) where
  pretty (ModuleHead _ name Nothing Nothing) = do
    s "module "
    pretty name
    s " where"
  pretty _ = throwError "unhandled ModuleHead"

instance Pretty (ModuleName ()) where
  pretty (ModuleName _ x) = s x

instance Pretty (ImportDecl ()) where
  pretty (ImportDecl () mod qualified False False Nothing as spec) = do
    s "import "
    when qualified $ s "qualified "
    pretty mod
    forM_ as $ \name -> do
      s " as "
      pretty name
    pretty spec
  pretty _ = throwError "unhandled ImportDecl"

instance Pretty (Maybe (ImportSpecList ())) where
  pretty x =
    forM_ x $ \(ImportSpecList _ b l) -> do
      s " "
      when b $ s "hiding "
      parens $
        sequence_ $
          intersperse (s ", ") $
            map pretty l

instance Pretty (ImportSpec ()) where
  pretty x =
    attemptAll [
      prettyPrism _IVar' x,
      prettyPrism _IAbs' x,
      prettyPrism _IThingAll' x,
      prettyPrism _IThingWith' x
    ]

instance Pretty (C_IVar ()) where
  pretty (C_IVar ((), n)) = pretty n

instance Pretty (Name ()) where
  pretty x =
    attemptAll [
      prettyPrism _Ident' x,
      prettyPrism _Symbol' x
    ]

instance Pretty (C_Ident ()) where
  pretty (C_Ident ((), name)) = s name

instance Pretty (Decl ()) where
  pretty x =
    attemptAll [
      prettyPrism _TypeDecl' x,
      prettyPrism _TypeFamDecl' x,
      prettyPrism _ClosedTypeFamDecl' x,
      prettyPrism _DataDecl' x,
      prettyPrism _GDataDecl' x,
      prettyPrism _DataFamDecl' x,
      prettyPrism _TypeInsDecl' x,
      prettyPrism _DataInsDecl' x,
      prettyPrism _GDataInsDecl' x,
      prettyPrism _ClassDecl' x,
      prettyPrism _InstDecl' x,
      prettyPrism _DerivDecl' x,
      prettyPrism _InfixDecl' x,
      prettyPrism _DefaultDecl' x,
      prettyPrism _SpliceDecl' x,
      prettyPrism _TypeSig' x,
      prettyPrism _PatSynSig' x,
      prettyPrism _FunBind' x,
      prettyPrism _PatBind' x,
      prettyPrism _PatSyn' x,
      prettyPrism _ForImp' x,
      prettyPrism _ForExp' x,
      prettyPrism _RulePragmaDecl' x,
      prettyPrism _DeprPragmaDecl' x,
      prettyPrism _WarnPragmaDecl' x,
      prettyPrism _InlineSig' x,
      prettyPrism _InlineConlikeSig' x,
      prettyPrism _SpecSig' x,
      prettyPrism _SpecInlineSig' x,
      prettyPrism _InstSig' x,
      prettyPrism _AnnPragma' x,
      prettyPrism _MinimalPragma' x,
      prettyPrism _RoleAnnotDecl' x
    ]

instance Pretty (C_TypeSig ()) where
  pretty (C_TypeSig ((), [name], ty)) = do
    pretty name
    s " :: "
    pretty ty
  pretty _ = throwError "unhandled C_TypeSig"

instance Pretty (C_FunBind ()) where
  pretty (C_FunBind ((), matches)) = do
    sequence_ $
      intersperse newline $
        map pretty matches

instance Pretty (C_PatBind ()) where
  pretty (C_PatBind ((), pat, rhs, Nothing)) = do
    pretty pat
    s " = "
    pretty rhs

instance Pretty (Match ()) where
  pretty x =
    attemptAll [
      prettyPrism _Match' x,
      prettyPrism _InfixMatch' x
    ]

instance Pretty (C_Match ()) where
  pretty (C_Match ((), name, bindings, rhs, Nothing)) = do
    pretty name
    s " "
    forM_ bindings $ \binding -> do
      pretty binding
      s " "
    s "= "
    pretty rhs
  pretty _ = throwError "unhandled C_Match"

instance Pretty (Rhs ()) where
  pretty x =
    attemptAll [
      prettyPrism _UnGuardedRhs' x,
      prettyPrism _GuardedRhss' x
    ]

instance Pretty (C_UnGuardedRhs ()) where
  pretty (C_UnGuardedRhs ((), exp)) = pretty exp

instance Pretty [Pat ()] where
  pretty x =
    sequence_ $
      intersperse (s " ") $
        map pretty x

instance Pretty (Pat ()) where
  pretty x =
    attemptAll [
      prettyPrism _PVar' x,
      prettyPrism _PLit' x,
      prettyPrism _PNPlusK' x,
      prettyPrism _PInfixApp' x,
      prettyPrism _PApp' x,
      prettyPrism _PTuple' x,
      prettyPrism _PList' x,
      prettyPrism _PParen' x,
      prettyPrism _PRec' x,
      prettyPrism _PAsPat' x,
      prettyPrism _PWildCard' x,
      prettyPrism _PIrrPat' x,
      prettyPrism _PatTypeSig' x,
      prettyPrism _PViewPat' x,
      prettyPrism _PRPat' x,
      prettyPrism _PXTag' x,
      prettyPrism _PXETag' x,
      prettyPrism _PXPcdata' x,
      prettyPrism _PXPatTag' x,
      prettyPrism _PXRPats' x,
      prettyPrism _PQuasiQuote' x,
      prettyPrism _PBangPat' x
    ]

instance Pretty (C_PVar ()) where
  pretty (C_PVar ((), name)) = pretty name

instance Pretty (C_PParen ()) where
  pretty (C_PParen ((), x)) = parens $ pretty x

instance Pretty (C_PApp ()) where
  pretty (C_PApp ((), f, x)) = do
    pretty f
    s " "
    pretty x

instance Pretty (Exp ()) where
  pretty x =
    attemptAll [
      prettyPrism _Var' x,
      prettyPrism _OverloadedLabel' x,
      prettyPrism _IPVar' x,
      prettyPrism _Con' x,
      prettyPrism _Lit' x,
      prettyPrism _InfixApp' x,
      prettyPrism _App' x,
      prettyPrism _NegApp' x,
      prettyPrism _Lambda' x,
      prettyPrism _Let' x,
      prettyPrism _If' x,
      prettyPrism _MultiIf' x,
      prettyPrism _Case' x,
      prettyPrism _Do' x,
      prettyPrism _MDo' x,
      prettyPrism _Tuple' x,
      prettyPrism _TupleSection' x,
      prettyPrism _List' x,
      prettyPrism _ParArray' x,
      prettyPrism _Paren' x,
      prettyPrism _LeftSection' x,
      prettyPrism _RightSection' x,
      prettyPrism _RecConstr' x,
      prettyPrism _RecUpdate' x,
      prettyPrism _EnumFrom' x,
      prettyPrism _EnumFromTo' x,
      prettyPrism _EnumFromThen' x,
      prettyPrism _EnumFromThenTo' x,
      prettyPrism _ParArrayFromTo' x,
      prettyPrism _ParArrayFromThenTo' x,
      prettyPrism _ListComp' x,
      prettyPrism _ParComp' x,
      prettyPrism _ParArrayComp' x,
      prettyPrism _ExpTypeSig' x,
      prettyPrism _VarQuote' x,
      prettyPrism _TypQuote' x,
      prettyPrism _BracketExp' x,
      prettyPrism _SpliceExp' x,
      prettyPrism _QuasiQuote' x,
      prettyPrism _TypeApp' x,
      prettyPrism _XTag' x,
      prettyPrism _XETag' x,
      prettyPrism _XPcdata' x,
      prettyPrism _XExpTag' x,
      prettyPrism _XChildTag' x,
      prettyPrism _CorePragma' x,
      prettyPrism _SCCPragma' x,
      prettyPrism _GenPragma' x,
      prettyPrism _Proc' x,
      prettyPrism _LeftArrApp' x,
      prettyPrism _RightArrApp' x,
      prettyPrism _LeftArrHighApp' x,
      prettyPrism _RightArrHighApp' x,
      prettyPrism _LCase' x,
      prettyPrism _ExprHole' x
    ]

instance Pretty (C_Con ()) where
  pretty (C_Con ((), name)) = pretty name

instance Pretty (C_App ()) where
  pretty (C_App ((), f, x)) = do
    pretty f
    s " "
    pretty x

instance Pretty (C_Var ()) where
  pretty (C_Var ((), name)) = pretty name

instance Pretty (Type ()) where
  pretty x =
    attemptAll [
      prettyPrism _TyForall' x,
      prettyPrism _TyFun' x,
      prettyPrism _TyTuple' x,
      prettyPrism _TyList' x,
      prettyPrism _TyParArray' x,
      prettyPrism _TyApp' x,
      prettyPrism _TyVar' x,
      prettyPrism _TyCon' x,
      prettyPrism _TyParen' x,
      prettyPrism _TyInfix' x,
      prettyPrism _TyKind' x,
      prettyPrism _TyPromoted' x,
      prettyPrism _TyEquals' x,
      prettyPrism _TySplice' x,
      prettyPrism _TyBang' x,
      prettyPrism _TyWildCard' x,
      prettyPrism _TyQuasiQuote' x
    ]

instance Pretty (C_TyCon ()) where
  pretty (C_TyCon ((), name)) = pretty name

instance Pretty (QName ()) where
  pretty x =
    attemptAll [
      prettyPrism _Qual' x,
      prettyPrism _UnQual' x,
      prettyPrism _Special' x
    ]

instance Pretty (C_UnQual ()) where
  pretty (C_UnQual ((), name)) = pretty name
