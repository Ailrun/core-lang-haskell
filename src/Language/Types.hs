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
  deriving (Show)
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
