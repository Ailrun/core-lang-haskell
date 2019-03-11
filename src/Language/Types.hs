module Language.Types where

data Expr a
  = EVar Name
  | ENum Int
  | EConstr Int Int
  | EAp (Expr a) (Expr a)
  | ELet
    IsRec
    [(a, Expr a)]
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

bindersOf :: [(a, b)] -> [a]
bindersOf = map fst
rhssOf :: [(a, b)] -> [b]
rhssOf = map snd

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
