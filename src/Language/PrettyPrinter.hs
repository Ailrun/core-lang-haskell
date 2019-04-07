{-# LANGUAGE CPP #-}
module Language.PrettyPrinter where

import Data.ISeq
import Language.Types
import Util

prettyPrint :: CoreProgram -> String

-- |
-- Following codes show bad performances,
-- therefore they are impractical.
-- See </exercises/exercise1-01.xls exercise1-01.xls> for data.
#if __CLH_EXERCISE_1__ <= 1
prettyPrintExpr :: CoreExpr -> String
prettyPrintExpr (ENum n) = show n
prettyPrintExpr (EVar v) = v
prettyPrintExpr (EAp e1 e2) = prettyPrintExpr e1 ++ " " ++ prettyPrintAExpr e2

prettyPrintAExpr :: CoreExpr -> String
prettyPrintAExpr e
  | isAExpr e = prettyPrintExpr e
  | otherwise = "(" ++ prettyPrintExpr e ++ ")"

-- |
-- Utility for exercise 1.1
makeMultiAp :: Int -> CoreExpr -> CoreExpr -> CoreExpr
makeMultiAp n e1 e2 = foldl EAp e1 (take n e2s)
  where
    e2s = e2 : e2s
#endif

#if __CLH_EXERCISE_1__ >= 1
#if __CLH_EXERCISE_1__ < 8
prettyPrintExpr :: CoreExpr -> ISeq
#if __CLH_EXERCISE_1__ < 3
prettyPrintExpr (EVar v) = iStr v
prettyPrintExpr (EAp e1 e2) = prettyPrintExpr e1 `iAppend` iStr " " `iAppend` prettyPrintAExpr e2
prettyPrintExpr (ELet isRec defns expr)
  = iConcat [ iStr keyword, iNewline
            , iStr "  ", iIndent (prettyPrintDefinitions defns), iNewline
            , iStr "in ", prettyPrintExpr expr
            ]
  where
    keyword
      | not isRec = "let"
      | otherwise = "letrec"
#endif

prettyPrintDefinitions :: Assoc Name CoreExpr -> ISeq
prettyPrintDefinitions defns
  = iInterleave sep (map prettyPrintDefinition defns)
  where
    sep = iConcat [ iStr ";", iNewline ]

prettyPrintDefinition :: (Name, CoreExpr) -> ISeq
prettyPrintDefinition (name, expr)
  = iConcat [ iStr name, iStr " = ", iIndent (prettyPrintExpr expr) ]

-- |
-- Following two patterns of 'prettyPrintExpr',
-- 'prettyPrintAlternatives', 'prettyPrintAlternative',
-- 'prettyPrintVars', 'prettyPrintAExpr',
-- 'prettyPrintProgram' and 'prettyPrintSupercombinatorDefinition'
-- are exercise 1.3
#if __CLH_EXERCISE_1__ >= 3
prettyPrintExpr (EVar v) = iStr v
prettyPrintExpr (EAp e1 e2) = prettyPrintExpr e1 `iAppend` iStr " " `iAppend` prettyPrintAExpr e2
#endif
prettyPrintExpr (ELet isRec defns expr)
  = iConcat [ iStr keyword, iNewline
            , iStr "  ", iIndent (prettyPrintDefinitions defns), iNewline
            , iStr "in ", prettyPrintExpr expr
            ]
  where
    keyword
      | not isRec = "let"
      | otherwise = "letrec"
prettyPrintExpr (ECase expr alters)
  = iConcat [ iStr "case ", iIndent (prettyPrintExpr expr), iStr " of", iNewline
            , iStr "  ", iIndent (prettyPrintAlternatives alters)
            ]
prettyPrintExpr (ELam vars expr)
  = iConcat [ iStr "\\ ", prettyPrintVars vars, iStr " .", iNewline
            , iStr "  ", iIndent (prettyPrintExpr expr)
            ]

#if __CLH_EXERCISE_1__ >= 3
prettyPrintAlternatives :: [CoreAlter] -> ISeq
prettyPrintAlternatives alters
  = iInterleave sep (map prettyPrintAlternative alters)
  where
    sep = iConcat [ iStr ";", iNewline ]

prettyPrintAlternative :: CoreAlter -> ISeq
prettyPrintAlternative (tag, [], expr)
  = iConcat [ iStr "<", iStr (show tag), iStr "> -> ", iIndent (prettyPrintExpr expr) ]
prettyPrintAlternative (tag, vars, expr)
  = iConcat [ iStr "<", iStr (show tag), iStr "> ", prettyPrintVars vars, iStr " -> ", iIndent (prettyPrintExpr expr) ]
#endif
#endif

#if __CLH_EXERCISE_1__ >= 3
prettyPrintVars :: [Name] -> ISeq
prettyPrintVars vars
  = iInterleave (iStr " ") (map iStr vars)

#if __CLH_EXERCISE_1__ < 8
prettyPrintAExpr :: CoreExpr -> ISeq
prettyPrintAExpr expr
  | isAExpr expr = prettyPrintExpr expr
  | otherwise = iConcat [ iStr "(", prettyPrintExpr expr, iStr ")" ]
#endif

prettyPrintProgram :: CoreProgram -> ISeq
prettyPrintProgram scdefns
  = iInterleave sep (map prettyPrintSupercombinatorDefinition scdefns)
  where
    sep = iConcat [ iStr ";", iNewline ]

prettyPrintSupercombinatorDefinition :: CoreScDefn -> ISeq
prettyPrintSupercombinatorDefinition (name, [], expr)
  = iConcat [ iStr name, iStr " = ", iIndent (prettyPrintExpr 0 expr) ]
prettyPrintSupercombinatorDefinition (name, vars, expr)
  = iConcat [ iStr name, iStr " ", prettyPrintVars vars, iStr " = ", iIndent (prettyPrintExpr 0 expr) ]

-- |
-- 'prettyPrintExpr' with 'ISep' works much faster than one without it.
-- See </exercises/exercise1-04.xls exercise1-04.xls> for data.
#if __CLH_EXERCISE_1__ == 4
makeMultiAp :: Int -> CoreExpr -> CoreExpr -> CoreExpr
makeMultiAp n e1 e2 = foldl EAp e1 (take n e2s)
  where
    e2s = e2 : e2s
#endif

#if __CLH_EXERCISE_1__ >= 8
prettyPrintExpr :: Int -> CoreExpr -> ISeq
prettyPrintExpr _ (ENum n) = iNum n
prettyPrintExpr _ (EVar v) = iStr v
prettyPrintExpr prec (EAp (EAp (EVar "|") e1) e2)
  = iPrecParen prec 1 . iConcat $ [ prettyPrintExpr 2 e1, iStr " | ", prettyPrintExpr 1 e2 ]
prettyPrintExpr prec (EAp (EAp (EVar "&") e1) e2)
  = iPrecParen prec 2 . iConcat $ [ prettyPrintExpr 3 e1, iStr " & ", prettyPrintExpr 2 e2 ]
prettyPrintExpr prec (EAp (EAp (EVar "==") e1) e2)
  = iPrecParen prec 3 . iConcat $ [ prettyPrintExpr 4 e1, iStr " == ", prettyPrintExpr 4 e2 ]
prettyPrintExpr prec (EAp (EAp (EVar "~=") e1) e2)
  = iPrecParen prec 3 . iConcat $ [ prettyPrintExpr 4 e1, iStr " ~= ", prettyPrintExpr 4 e2 ]
prettyPrintExpr prec (EAp (EAp (EVar ">") e1) e2)
  = iPrecParen prec 3 . iConcat $ [ prettyPrintExpr 4 e1, iStr " > ", prettyPrintExpr 4 e2 ]
prettyPrintExpr prec (EAp (EAp (EVar ">=") e1) e2)
  = iPrecParen prec 3 . iConcat $ [ prettyPrintExpr 4 e1, iStr " >= ", prettyPrintExpr 4 e2 ]
prettyPrintExpr prec (EAp (EAp (EVar "<") e1) e2)
  = iPrecParen prec 3 . iConcat $ [ prettyPrintExpr 4 e1, iStr " < ", prettyPrintExpr 4 e2 ]
prettyPrintExpr prec (EAp (EAp (EVar "<=") e1) e2)
  = iPrecParen prec 3 . iConcat $ [ prettyPrintExpr 4 e1, iStr " <= ", prettyPrintExpr 4 e2 ]
prettyPrintExpr prec (EAp (EAp (EVar "+") e1) e2)
  = iPrecParen prec 4 . iConcat $ [ prettyPrintExpr 5 e1, iStr " + ", prettyPrintExpr 4 e2 ]
prettyPrintExpr prec (EAp (EAp (EVar "-") e1) e2)
  = iPrecParen prec 4 . iConcat $ [ prettyPrintExpr 5 e1, iStr " - ", prettyPrintExpr 5 e2 ]
prettyPrintExpr prec (EAp (EAp (EVar "*") e1) e2)
  = iPrecParen prec 5 . iConcat $ [ prettyPrintExpr 6 e1, iStr " * ", prettyPrintExpr 5 e2 ]
prettyPrintExpr prec (EAp (EAp (EVar "/") e1) e2)
  = iPrecParen prec 5 . iConcat $ [ prettyPrintExpr 6 e1, iStr " / ", prettyPrintExpr 6 e2 ]
prettyPrintExpr prec (EAp e1 e2)
  = iPrecParen prec 10 . iConcat $ [ prettyPrintExpr 10 e1, iStr " ", prettyPrintExpr 11 e2 ]
prettyPrintExpr _ (ELet isRec defns expr)
  = iConcat [ iStr keyword, iNewline
            , iStr "  ", iIndent (prettyPrintDefinitions defns), iNewline
            , iStr "in ", prettyPrintExpr 0 expr
            ]
  where
    keyword
      | not isRec = "let"
      | otherwise = "letrec"
prettyPrintExpr _ (ECase expr alters)
  = iConcat [ iStr "case ", iIndent (prettyPrintExpr 0 expr), iStr " of", iNewline
            , iStr "  ", iIndent (prettyPrintAlternatives alters)
            ]
prettyPrintExpr _ (ELam vars expr)
  = iConcat [ iStr "\\ ", prettyPrintVars vars, iStr " .", iNewline
            , iStr "  ", iIndent (prettyPrintExpr 0 expr)
            ]
prettyPrintExpr _ (EConstr t a)
  = iConcat [ iStr "Pack{", iNum t, iStr ",", iNum a, iStr "}" ]

prettyPrintAlternatives :: [CoreAlter] -> ISeq
prettyPrintAlternatives alters
  = iInterleave sep (map prettyPrintAlternative alters)
  where
    sep = iConcat [ iStr ";", iNewline ]

prettyPrintAlternative :: CoreAlter -> ISeq
prettyPrintAlternative (tag, [], expr)
  = iConcat [ iStr "<", iStr (show tag), iStr "> -> ", iIndent (prettyPrintExpr 0 expr) ]
prettyPrintAlternative (tag, vars, expr)
  = iConcat [ iStr "<", iStr (show tag), iStr "> ", prettyPrintVars vars, iStr " -> ", iIndent (prettyPrintExpr 0 expr) ]

prettyPrintDefinitions :: Assoc Name CoreExpr -> ISeq
prettyPrintDefinitions defns
  = iInterleave sep (map prettyPrintDefinition defns)
  where
    sep = iConcat [ iStr ";", iNewline ]

prettyPrintDefinition :: (Name, CoreExpr) -> ISeq
prettyPrintDefinition (name, expr)
  = iConcat [ iStr name, iStr " = ", iIndent (prettyPrintExpr 0 expr) ]

prettyPrint program = iDisplay (prettyPrintProgram program)

#if __CLH_EXERCISE_6__ >= 1
prettyPrintGen :: (a -> ISeq) -> Program a -> String
prettyPrintGen f program = iDisplay (prettyPrintProgramGen f program)

prettyPrintProgramGen :: (a -> ISeq) -> Program a -> ISeq
prettyPrintProgramGen f scdefns
  = iInterleave sep (map (prettyPrintSupercombinatorDefinitionGen f) scdefns)
  where
    sep = iConcat [ iStr ";", iNewline ]

prettyPrintSupercombinatorDefinitionGen :: (a -> ISeq) -> ScDefn a -> ISeq
prettyPrintSupercombinatorDefinitionGen f (name, [], expr)
  = iConcat [ iStr name, iStr " = ", iIndent (prettyPrintExprGen f 0 expr) ]
prettyPrintSupercombinatorDefinitionGen f (name, vars, expr)
  = iConcat [ iStr name, iStr " ", prettyPrintVarsGen f vars, iStr " = ", iIndent (prettyPrintExprGen f 0 expr) ]

prettyPrintExprGen :: (a -> ISeq) -> Int -> Expr a -> ISeq
prettyPrintExprGen _ _ (ENum n) = iNum n
prettyPrintExprGen _ _ (EVar v) = iStr v
prettyPrintExprGen f prec (EAp (EAp (EVar "|") e1) e2)
  = iPrecParen prec 1 . iConcat $ [ prettyPrintExprGen f 2 e1, iStr " | ", prettyPrintExprGen f 1 e2 ]
prettyPrintExprGen f prec (EAp (EAp (EVar "&") e1) e2)
  = iPrecParen prec 2 . iConcat $ [ prettyPrintExprGen f 3 e1, iStr " & ", prettyPrintExprGen f 2 e2 ]
prettyPrintExprGen f prec (EAp (EAp (EVar "==") e1) e2)
  = iPrecParen prec 3 . iConcat $ [ prettyPrintExprGen f 4 e1, iStr " == ", prettyPrintExprGen f 4 e2 ]
prettyPrintExprGen f prec (EAp (EAp (EVar "~=") e1) e2)
  = iPrecParen prec 3 . iConcat $ [ prettyPrintExprGen f 4 e1, iStr " ~= ", prettyPrintExprGen f 4 e2 ]
prettyPrintExprGen f prec (EAp (EAp (EVar ">") e1) e2)
  = iPrecParen prec 3 . iConcat $ [ prettyPrintExprGen f 4 e1, iStr " > ", prettyPrintExprGen f 4 e2 ]
prettyPrintExprGen f prec (EAp (EAp (EVar ">=") e1) e2)
  = iPrecParen prec 3 . iConcat $ [ prettyPrintExprGen f 4 e1, iStr " >= ", prettyPrintExprGen f 4 e2 ]
prettyPrintExprGen f prec (EAp (EAp (EVar "<") e1) e2)
  = iPrecParen prec 3 . iConcat $ [ prettyPrintExprGen f 4 e1, iStr " < ", prettyPrintExprGen f 4 e2 ]
prettyPrintExprGen f prec (EAp (EAp (EVar "<=") e1) e2)
  = iPrecParen prec 3 . iConcat $ [ prettyPrintExprGen f 4 e1, iStr " <= ", prettyPrintExprGen f 4 e2 ]
prettyPrintExprGen f prec (EAp (EAp (EVar "+") e1) e2)
  = iPrecParen prec 4 . iConcat $ [ prettyPrintExprGen f 5 e1, iStr " + ", prettyPrintExprGen f 4 e2 ]
prettyPrintExprGen f prec (EAp (EAp (EVar "-") e1) e2)
  = iPrecParen prec 4 . iConcat $ [ prettyPrintExprGen f 5 e1, iStr " - ", prettyPrintExprGen f 5 e2 ]
prettyPrintExprGen f prec (EAp (EAp (EVar "*") e1) e2)
  = iPrecParen prec 5 . iConcat $ [ prettyPrintExprGen f 6 e1, iStr " * ", prettyPrintExprGen f 5 e2 ]
prettyPrintExprGen f prec (EAp (EAp (EVar "/") e1) e2)
  = iPrecParen prec 5 . iConcat $ [ prettyPrintExprGen f 6 e1, iStr " / ", prettyPrintExprGen f 6 e2 ]
prettyPrintExprGen f prec (EAp e1 e2)
  = iPrecParen prec 10 . iConcat $ [ prettyPrintExprGen f 10 e1, iStr " ", prettyPrintExprGen f 11 e2 ]
prettyPrintExprGen f _ (ELet isRec defns expr)
  = iConcat [ iStr keyword, iNewline
            , iStr "  ", iIndent (prettyPrintDefinitionsGen f defns), iNewline
            , iStr "in ", prettyPrintExprGen f 0 expr
            ]
  where
    keyword
      | not isRec = "let"
      | otherwise = "letrec"
prettyPrintExprGen f _ (ECase expr alters)
  = iConcat [ iStr "case ", iIndent (prettyPrintExprGen f 0 expr), iStr " of", iNewline
            , iStr "  ", iIndent (prettyPrintAlternativesGen f alters)
            ]
prettyPrintExprGen f _ (ELam vars expr)
  = iConcat [ iStr "\\ ", prettyPrintVarsGen f vars, iStr " .", iNewline
            , iStr "  ", iIndent (prettyPrintExprGen f 0 expr)
            ]
prettyPrintExprGen _ _ (EConstr t a)
  = iConcat [ iStr "Pack{", iNum t, iStr ",", iNum a, iStr "}" ]

prettyPrintDefinitionsGen :: (a -> ISeq) -> Assoc a (Expr a) -> ISeq
prettyPrintDefinitionsGen f defns
  = iInterleave sep (map (prettyPrintDefinitionGen f) defns)
  where
    sep = iConcat [ iStr ";", iNewline ]

prettyPrintDefinitionGen :: (a -> ISeq) -> (a, Expr a) -> ISeq
prettyPrintDefinitionGen f (name, expr)
  = iConcat [ f name, iStr " = ", iIndent (prettyPrintExprGen f 0 expr) ]


prettyPrintAlternativesGen :: (a -> ISeq) -> [Alter a] -> ISeq
prettyPrintAlternativesGen f alters
  = iInterleave sep (map (prettyPrintAlternativeGen f) alters)
  where
    sep = iConcat [ iStr ";", iNewline ]

prettyPrintAlternativeGen :: (a -> ISeq) -> Alter a -> ISeq
prettyPrintAlternativeGen f (tag, [], expr)
  = iConcat [ iStr "<", iStr (show tag), iStr "> -> ", iIndent (prettyPrintExprGen f 0 expr) ]
prettyPrintAlternativeGen f (tag, vars, expr)
  = iConcat [ iStr "<", iStr (show tag), iStr "> ", prettyPrintVarsGen f vars, iStr " -> ", iIndent (prettyPrintExprGen f 0 expr) ]

prettyPrintVarsGen :: (a -> ISeq) -> [a] -> ISeq
prettyPrintVarsGen f vars
  = iInterleave (iStr " ") (map f vars)

#if __CLH_EXERCISE_6__ >= 2
prettyPrintAnnGen :: (a -> ISeq) -> (b -> ISeq) -> AnnProgram a b -> String
prettyPrintAnnGen f g program = iDisplay (prettyPrintAnnProgramGen f g program)

prettyPrintAnnProgramGen :: (a -> ISeq) -> (b -> ISeq) -> AnnProgram a b -> ISeq
prettyPrintAnnProgramGen f g scdefns
  = iInterleave sep (map (prettyPrintAnnSupercombinatorDefinitionGen f g) scdefns)
  where
    sep = iConcat [ iStr ";", iNewline ]

prettyPrintAnnSupercombinatorDefinitionGen :: (a -> ISeq) -> (b -> ISeq) -> AnnScDefn a b -> ISeq
prettyPrintAnnSupercombinatorDefinitionGen f g (name, [], expr)
  = iConcat [ iStr name, iStr " = ", iIndent (prettyPrintAnnExprGen f g 0 expr) ]
prettyPrintAnnSupercombinatorDefinitionGen f g (name, vars, expr)
  = iConcat [ iStr name, iStr " ", prettyPrintAnnVarsGen f g vars, iStr " = ", iIndent (prettyPrintAnnExprGen f g 0 expr) ]

prettyPrintAnnExprGen :: (a -> ISeq) -> (b -> ISeq) -> Int -> AnnExpr a b -> ISeq
prettyPrintAnnExprGen f g prec (b, expr')
  = prettyPrintWith g b (prettyPrintAnnExpr'Gen f g prec expr')

prettyPrintAnnExpr'Gen :: (a -> ISeq) -> (b -> ISeq) -> Int -> AnnExpr' a b -> ISeq
prettyPrintAnnExpr'Gen _ _ _ (ANum n) = iNum n
prettyPrintAnnExpr'Gen _ _ _ (AVar v) = iStr v
prettyPrintAnnExpr'Gen f g prec (AAp (b, AAp eV@(_, AVar "|") e1) e2)
  = iPrecParen prec 1 . prettyPrintWith g b . iConcat $ [ prettyPrintAnnExprGen f g 2 e1, iStr " ", prettyPrintAnnExprGen f g 0 eV, iStr " ", prettyPrintAnnExprGen f g 1 e2 ]
prettyPrintAnnExpr'Gen f g prec (AAp (b, AAp eV@(_, AVar "&") e1) e2)
  = iPrecParen prec 2 . prettyPrintWith g b . iConcat $ [ prettyPrintAnnExprGen f g 3 e1, iStr " ", prettyPrintAnnExprGen f g 0 eV, iStr " ", prettyPrintAnnExprGen f g 2 e2 ]
prettyPrintAnnExpr'Gen f g prec (AAp (b, AAp eV@(_, AVar "==") e1) e2)
  = iPrecParen prec 3 . prettyPrintWith g b . iConcat $ [ prettyPrintAnnExprGen f g 4 e1, iStr " ", prettyPrintAnnExprGen f g 0 eV, iStr " ", prettyPrintAnnExprGen f g 4 e2 ]
prettyPrintAnnExpr'Gen f g prec (AAp (b, AAp eV@(_, AVar "~=") e1) e2)
  = iPrecParen prec 3 . prettyPrintWith g b . iConcat $ [ prettyPrintAnnExprGen f g 4 e1, iStr " ", prettyPrintAnnExprGen f g 0 eV, iStr " ", prettyPrintAnnExprGen f g 4 e2 ]
prettyPrintAnnExpr'Gen f g prec (AAp (b, AAp eV@(_, AVar ">") e1) e2)
  = iPrecParen prec 3 . prettyPrintWith g b . iConcat $ [ prettyPrintAnnExprGen f g 4 e1, iStr " ", prettyPrintAnnExprGen f g 0 eV, iStr " ", prettyPrintAnnExprGen f g 4 e2 ]
prettyPrintAnnExpr'Gen f g prec (AAp (b, AAp eV@(_, AVar ">=") e1) e2)
  = iPrecParen prec 3 . prettyPrintWith g b . iConcat $ [ prettyPrintAnnExprGen f g 4 e1, iStr " ", prettyPrintAnnExprGen f g 0 eV, iStr " ", prettyPrintAnnExprGen f g 4 e2 ]
prettyPrintAnnExpr'Gen f g prec (AAp (b, AAp eV@(_, AVar "<") e1) e2)
  = iPrecParen prec 3 . prettyPrintWith g b . iConcat $ [ prettyPrintAnnExprGen f g 4 e1, iStr " ", prettyPrintAnnExprGen f g 0 eV, iStr " ", prettyPrintAnnExprGen f g 4 e2 ]
prettyPrintAnnExpr'Gen f g prec (AAp (b, AAp eV@(_, AVar "<=") e1) e2)
  = iPrecParen prec 3 . prettyPrintWith g b . iConcat $ [ prettyPrintAnnExprGen f g 4 e1, iStr " ", prettyPrintAnnExprGen f g 0 eV, iStr " ", prettyPrintAnnExprGen f g 4 e2 ]
prettyPrintAnnExpr'Gen f g prec (AAp (b, AAp eV@(_, AVar "+") e1) e2)
  = iPrecParen prec 4 . prettyPrintWith g b . iConcat $ [ prettyPrintAnnExprGen f g 5 e1, iStr " ", prettyPrintAnnExprGen f g 0 eV, iStr " ", prettyPrintAnnExprGen f g 4 e2 ]
prettyPrintAnnExpr'Gen f g prec (AAp (b, AAp eV@(_, AVar "-") e1) e2)
  = iPrecParen prec 4 . prettyPrintWith g b . iConcat $ [ prettyPrintAnnExprGen f g 5 e1, iStr " ", prettyPrintAnnExprGen f g 0 eV, iStr " ", prettyPrintAnnExprGen f g 5 e2 ]
prettyPrintAnnExpr'Gen f g prec (AAp (b, AAp eV@(_, AVar "*") e1) e2)
  = iPrecParen prec 5 . prettyPrintWith g b . iConcat $ [ prettyPrintAnnExprGen f g 6 e1, iStr " ", prettyPrintAnnExprGen f g 0 eV, iStr " ", prettyPrintAnnExprGen f g 5 e2 ]
prettyPrintAnnExpr'Gen f g prec (AAp (b, AAp eV@(_, AVar "/") e1) e2)
  = iPrecParen prec 5 . prettyPrintWith g b . iConcat $ [ prettyPrintAnnExprGen f g 6 e1, iStr " ", prettyPrintAnnExprGen f g 0 eV, iStr " ", prettyPrintAnnExprGen f g 6 e2 ]
prettyPrintAnnExpr'Gen f g prec (AAp e1 e2)
  = iPrecParen prec 10 . iConcat $ [ prettyPrintAnnExprGen f g 10 e1, iStr " ", prettyPrintAnnExprGen f g 11 e2 ]
prettyPrintAnnExpr'Gen f g _ (ALet isRec defns expr)
  = iConcat [ iStr keyword, iNewline
            , iStr "  ", iIndent (prettyPrintAnnDefinitionsGen f g defns), iNewline
            , iStr "in ", prettyPrintAnnExprGen f g 0 expr
            ]
  where
    keyword
      | not isRec = "let"
      | otherwise = "letrec"
prettyPrintAnnExpr'Gen f g _ (ACase expr alters)
  = iConcat [ iStr "case ", iIndent (prettyPrintAnnExprGen f g 0 expr), iStr " of", iNewline
            , iStr "  ", iIndent (prettyPrintAnnAlternativesGen f g alters)
            ]
prettyPrintAnnExpr'Gen f g _ (ALam vars expr)
  = iConcat [ iStr "\\ ", prettyPrintAnnVarsGen f g vars, iStr " .", iNewline
            , iStr "  ", iIndent (prettyPrintAnnExprGen f g 0 expr)
            ]
prettyPrintAnnExpr'Gen _ _ _ (AConstr t a)
  = iConcat [ iStr "Pack{", iNum t, iStr ",", iNum a, iStr "}" ]

prettyPrintAnnDefinitionsGen :: (a -> ISeq) -> (b -> ISeq) -> Assoc a (AnnExpr a b) -> ISeq
prettyPrintAnnDefinitionsGen f g defns
  = iInterleave sep (map (prettyPrintAnnDefinitionGen f g) defns)
  where
    sep = iConcat [ iStr ";", iNewline ]

prettyPrintAnnDefinitionGen :: (a -> ISeq) -> (b -> ISeq) -> (a, AnnExpr a b) -> ISeq
prettyPrintAnnDefinitionGen f g (name, expr)
  = iConcat [ f name, iStr " = ", iIndent (prettyPrintAnnExprGen f g 0 expr) ]


prettyPrintAnnAlternativesGen :: (a -> ISeq) -> (b -> ISeq) -> [AnnAlter a b] -> ISeq
prettyPrintAnnAlternativesGen f g alters
  = iInterleave sep (map (prettyPrintAnnAlternativeGen f g) alters)
  where
    sep = iConcat [ iStr ";", iNewline ]

prettyPrintAnnAlternativeGen :: (a -> ISeq) -> (b -> ISeq) -> AnnAlter a b -> ISeq
prettyPrintAnnAlternativeGen f g (tag, [], expr)
  = iConcat [ iStr "<", iStr (show tag), iStr "> -> ", iIndent (prettyPrintAnnExprGen f g 0 expr) ]
prettyPrintAnnAlternativeGen f g (tag, vars, expr)
  = iConcat [ iStr "<", iStr (show tag), iStr "> ", prettyPrintAnnVarsGen f g vars, iStr " -> ", iIndent (prettyPrintAnnExprGen f g 0 expr) ]

prettyPrintAnnVarsGen :: (a -> ISeq) -> (b -> ISeq) -> [a] -> ISeq
prettyPrintAnnVarsGen f g vars
  = iInterleave (iStr " ") (map f vars)

prettyPrintWith :: (a -> ISeq) -> a -> ISeq -> ISeq
prettyPrintWith f a seq
  = iConcat [ iStr "(", f a, iStr ", ", seq, iStr ")" ]
#endif
#endif
#endif
#endif
#else
prettyPrint = undefined
#endif
