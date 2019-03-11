module Language.PrettyPrinter where

import Prelude hiding ( seq )

import Data.List
import Language.Types
import Util

prettyPrint :: CoreProgram -> String

-- |
-- Following codes show bad performances,
-- therefore they are impractical.
-- See /exercises/exercise1-1.xls for data.
{-
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
-}

iNil :: ISeq
iStr :: String -> ISeq
iAppend :: ISeq -> ISeq -> ISeq
iNewline :: ISeq
iIndent :: ISeq -> ISeq
iDisplay :: ISeq -> String

-- |
-- Before exercise 1.8
{-
prettyPrintExpr :: CoreExpr -> ISeq
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
-- |
-- Following two patterns of 'prettyPrintExpr',
-- 'prettyPrintAlternatives', 'prettyPrintAlternative' and 'prettyPrintVars'
-- are parts of exercise 1.3
prettyPrintExpr (ECase expr alters)
  = iConcat [ iStr "case ", iIndent (prettyPrintExpr expr), iStr " of", iNewline
            , iStr "  ", iIndent (prettyPrintAlternatives alters)
            ]
prettyPrintExpr (ELam vars expr)
  = iConcat [ iStr "\\ ", prettyPrintVars vars, iStr " .", iNewline
            , iStr "  ", iIndent (prettyPrintExpr expr)
            ]
-}
-- |
-- Following 'prettyPrintExpr' implementation is a part of exercise 1.8
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

-- |
-- Before exercise 1.8
{-
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
-}
-- |
-- Following 'prettyPrintAlternatives' and 'prettyPrintAlternative' implementations are parts of exercise 1.8
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

prettyPrintVars :: [Name] -> ISeq
prettyPrintVars vars
  = iInterleave (iStr " ") (map iStr vars)

-- |
-- Before exercise 1.8
{-
prettyPrintDefinitions :: [(Name, CoreExpr)] -> ISeq
prettyPrintDefinitions defns
  = iInterleave sep (map prettyPrintDefinition defns)
  where
    sep = iConcat [ iStr ";", iNewline ]

prettyPrintDefinition :: (Name, CoreExpr) -> ISeq
prettyPrintDefinition (name, expr)
  = iConcat [ iStr name, iStr " = ", iIndent (prettyPrintExpr expr) ]
-}
-- |
-- Following 'prettyPrintDefinitions' and 'prettyPrintDefinition' implementations are parts of exercise 1.8
prettyPrintDefinitions :: [(Name, CoreExpr)] -> ISeq
prettyPrintDefinitions defns
  = iInterleave sep (map prettyPrintDefinition defns)
  where
    sep = iConcat [ iStr ";", iNewline ]

prettyPrintDefinition :: (Name, CoreExpr) -> ISeq
prettyPrintDefinition (name, expr)
  = iConcat [ iStr name, iStr " = ", iIndent (prettyPrintExpr 0 expr) ]
-- |
-- Following 'iConcat' and 'iInterleave' implementations are exercise 1.2
iConcat :: [ISeq] -> ISeq
iConcat = foldl iAppend iNil

iInterleave :: ISeq -> [ISeq] -> ISeq
iInterleave _ [] = iNil
iInterleave sep (seq : seqs) = foldl (\acc s -> acc `iAppend` sep `iAppend` s) seq seqs

-- |
-- Following 'iPrecParen' implementation is a part of exercise 1.8
iPrecParen :: Int -> Int -> ISeq -> ISeq
iPrecParen contextPrec currentPrec seq
  | contextPrec > currentPrec = iConcat [ iStr "(", seq, iStr ")" ]
  | otherwise = seq

prettyPrint program = iDisplay (prettyPrintProgram program)

-- |
-- Before exercise 1.8
{-
-- |
-- Following 'prettyPrintAExpr', 'prettyPrintProgram' and
-- 'prettyPrintSupercombinatorDefinition' are parts of exercise 1.3
prettyPrintAExpr :: CoreExpr -> ISeq
prettyPrintAExpr expr
  | isAExpr expr = prettyPrintExpr expr
  | otherwise = iConcat [ iStr "(", prettyPrintExpr expr, iStr ")" ]
-}

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
-- Following definitions do not support indenting.
{-
data ISeq
  = INil
  | IStr String
  | IAppend ISeq ISeq
-}

iNil = INil
-- |
-- Before exercise 1.7
{-
iStr = IStr
-}
-- |
-- Following implementation of 'iStr' is exercise 1.7
iStr = iConcat . intersperse INewline . map IStr . lines
-- |
-- Before exercise 1.5
{-
iAppend = IAppend
-}
-- |
-- Following implementation of 'iAppend' is exercise 1.5
iAppend INil INil = INil
iAppend INil seq2 = seq2
iAppend seq1 INil = seq1
iAppend seq1 seq2 = IAppend seq1 seq2
-- |
-- Following definitions do not support indenting.
{-
iIndent seq = seq
iNewline = IStr "\n"

flatten :: [ISeq] -> String

iDisplay = flatten . return

flatten [] = ""
flatten (INil : seqs) = flatten seqs
flatten (IStr s : seqs) = s ++ flatten seqs
flatten (IAppend seq1 seq2 : seqs) = flatten (seq1 : seq2 : seqs)
-}

-- |
-- 'prettyPrintExpr' with 'ISep' works much faster than one without it.
-- See /exercises/exercise1-1.xls for data.
{-
-- |
-- Utility for exercise 1.4
makeMultiAp :: Int -> CoreExpr -> CoreExpr -> CoreExpr
makeMultiAp n e1 e2 = foldl EAp e1 (take n e2s)
  where
    e2s = e2 : e2s
-}

data ISeq
  = INil
  | IStr String
  | IAppend ISeq ISeq
  | IIndent ISeq
  | INewline

iIndent = IIndent
iNewline = INewline

flatten :: Int -> [(ISeq, Int)] -> String

iDisplay seq = flatten 0 [(seq, 0)]

flatten _ ((INewline, indent) : seqs)
  = '\n' : space indent ++ flatten indent seqs
flatten col ((IIndent seq, _) : seqs)
  = flatten col ((seq, col) : seqs)
-- |
-- Following patterns for 'flatten' are parts of exercise 1.6
flatten col ((IAppend seq1 seq2, indent) : seqs)
  = flatten col ((seq1, indent) : (seq2, indent) : seqs)
flatten col ((IStr s, _) : seqs) = s ++ flatten (col + length s) seqs
flatten col ((INil, _) : seqs) = flatten col seqs
flatten _ [] = ""

iNum :: Int -> ISeq
iNum = iStr . show

iFWNum :: Int -> Int -> ISeq
iFWNum width n
  = iStr (space (width - length digits) ++ digits)
  where
    digits = show n

-- |
-- This function is named after the similar function of the Miranda
iLayn :: [ISeq] -> ISeq
iLayn seqs
  = iConcat (zipWith layItem [1..] seqs)
  where
    layItem n seq
      = iConcat [ iFWNum 4 n, iStr ") ", iIndent seq, iNewline ]
