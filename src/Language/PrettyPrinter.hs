{-# LANGUAGE CPP #-}
module Language.PrettyPrinter where

import Data.ISeq
import Language.Types
import Util

prettyPrint :: CoreProgram -> String

-- |
-- Following codes show bad performances,
-- therefore they are impractical.
-- See </exercises/exercise1-1.xls exercise1-1.xls> for data.
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
-- See </exercises/exercise1-4.xls exercise1-4.xls> for data.
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
#endif
#endif
#else
prettyPrint = undefined
#endif
