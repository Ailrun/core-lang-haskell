{-# LANGUAGE CPP #-}
module Language.Types where

import Util

data Expr a
  = EVar Name
  | ENum Int
  | EConstr Int Int
  | EAp (Expr a) (Expr a)
  | ELet
    IsRec
    (Assoc a (Expr a))
    (Expr a)
  | ECase
    (Expr a)
    [Alter a]
  | ELam [a] (Expr a)
  deriving ( Show
           , Read
           , Eq
           )
type CoreExpr = Expr Name

type Name = String

type IsRec = Bool

recursive, nonRecursive :: IsRec
recursive = True
nonRecursive = False

bindersOf :: Assoc a b -> [a]
bindersOf = aDomain
rhssOf :: Assoc a b -> [b]
rhssOf = aRange

type Alter a = (Int, [a], Expr a)
type CoreAlter = Alter Name

isAExpr :: Expr a -> Bool
isAExpr (EVar _) = True
isAExpr (ENum _) = True
isAExpr _ = False

type Program a = [ScDefn a]
type CoreProgram = Program Name

type ScDefn a = (Name, [a], Expr a)
type CoreScDefn = ScDefn Name

#if __CLH_EXERCISE_6__ >= 1
data AnnExpr' a b
  = AVar Name
  | ANum Int
  | AConstr Int Int
  | AAp (AnnExpr a b) (AnnExpr a b)
  | ALet
    IsRec
    (Assoc a (AnnExpr a b))
    (AnnExpr a b)
  | ACase
    (AnnExpr a b)
    [AnnAlter a b]
  | ALam [a] (AnnExpr a b)
  deriving ( Show
           , Read
           , Eq
           )
type AnnExpr a b = (b, AnnExpr' a b)

type AnnAlter a b = (Int, [a], AnnExpr a b)

type AnnProgram a b = [AnnScDefn a b]

type AnnScDefn a b = (Name, [a], AnnExpr a b)
#endif
