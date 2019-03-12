{-# LANGUAGE CPP #-}
module Language.Parser where

import Data.Char
import Language.Types
import Util

#if __CLH_EXERCISE_1__ < 11
clex :: String -> [Token]
#endif
syntax :: [Token] -> CoreProgram

parse :: String -> CoreProgram
#if __CLH_EXERCISE_1__ < 11
parse = syntax . clex

type Token = String

clex (c : cs) | isSpace c = clex cs
clex (c : cs) | isDigit c = numToken : clex restCs
  where
    numToken = c : takeWhile isDigit cs
    restCs = dropWhile isDigit cs
clex (c : cs) | isAlpha c = varToken : clex restCs
  where
    varToken = c : takeWhile isIdChar cs
    restCs = dropWhile isIdChar cs
#if __CLH_EXERCISE_1__ >= 9
clex ('|' : '|' : cs) = clex restCs
  where
    restCs = dropWhile (`notElem` "\r\n") cs
#endif
#if __CLH_EXERCISE_1__ >= 10
clex (c0 : c1 : cs) | op `elem` twoCharOps = op : clex cs
  where
    op = [c0, c1]
#endif
clex (c : cs) = [c] : clex cs
clex [] = []
#endif

isIdChar c = isAlpha c || isDigit c || c == '_'

-- |
-- Following 'twoCharOps' is a part of exercise 1.10
twoCharOps :: [String]
twoCharOps = ["==", "~=", ">=", "<=", "->"]

#if __CLH_EXERCISE_1__ >= 11
parse = syntax . clex 0

type Token = (Int, String)

clex :: Int -> String -> [Token]
clex l ('\r' : '\n' : cs) = clex (l + 1) cs
clex l ('\n' : '\r' : cs) = clex (l + 1) cs
clex l ('\n' : cs) = clex (l + 1) cs
clex l (c : cs) | isSpace c = clex l cs
clex l (c : cs) | isDigit c = (l, numTokVal) : clex l restCs
  where
    numTokVal = c : takeWhile isDigit cs
    restCs = dropWhile isDigit cs
clex l (c : cs) | isAlpha c = (l, varTokVal) : clex l restCs
  where
    varTokVal = c : takeWhile isIdChar cs
    restCs = dropWhile isIdChar cs
clex l ('|' : '|' : cs) = clex l restCs
  where
    restCs = dropWhile (`notElem` "\r\n") cs
clex l (c0 : c1 : cs) | opTokVal `elem` twoCharOps = (l, opTokVal) : clex l cs
  where
    opTokVal = [c0, c1]
clex l (c : cs) = (l, [c]) : clex l cs
clex l [] = []

type Parser a = [Token] -> Assoc a [Token]

pLit :: String -> Parser String
#if __CLH_EXERCISE_1__ < 16
pLit s ((_, tokVal) : toks)
  | s == tokVal = [(s, toks)]
  | otherwise = []
pLit _ [] = []
#endif

pVar :: Parser String
#if __CLH_EXERCISE_1__ < 16
pVar ((_, c : cs) : toks)
  | isAlpha c = [(c : cs, toks)]
  | otherwise = []
pVar ((_, []) : _) = []
pVar [] = []
#endif

pAlt :: Parser a -> Parser a -> Parser a
pAlt p1 p2 toks = p1 toks ++ p2 toks

pThen :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
pThen combine p1 p2 toks
  = [ (combine v1 v2, toks2)
    | (v1, toks1) <- p1 toks
    , (v2, toks2) <- p2 toks1
    ]

#if __CLH_EXERCISE_1__ >= 12
pThen3 :: (a -> b -> c -> d) -> Parser a -> Parser b -> Parser c -> Parser d
pThen3 combine p1 p2 p3 toks
  = [ (combine v1 v2 v3, toks3)
    | (v1, toks1) <- p1 toks
    , (v2, toks2) <- p2 toks1
    , (v3, toks3) <- p3 toks2
    ]

pThen4 :: (a -> b -> c -> d -> e) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e
pThen4 combine p1 p2 p3 p4 toks
  = [ (combine v1 v2 v3 v4, toks4)
    | (v1, toks1) <- p1 toks
    , (v2, toks2) <- p2 toks1
    , (v3, toks3) <- p3 toks2
    , (v4, toks4) <- p4 toks3
    ]

#if __CLH_EXERCISE_1__ >= 13
pZeroOrMore :: Parser a -> Parser [a]
-- |
-- This one shows bad performance
-- since it always produces empty result even when
-- first parser succeeded.
#if __CLH_EXERCISE_1__ < 19
pZeroOrMore p = pOneOrMore p `pAlt` pEmpty []
#endif

pEmpty :: a -> Parser a
pEmpty v toks = [(v, toks)]

pOneOrMore :: Parser a -> Parser [a]
pOneOrMore p = pThen (:) p (pZeroOrMore p)

#if __CLH_EXERCISE_1__ >= 14
pApply :: Parser a -> (a -> b) -> Parser b
pApply p f toks
  = [ (f v', toks')
    | (v', toks') <- p toks
    ]

#if __CLH_EXERCISE_1__ >= 15
pOneOrMoreWithSep :: Parser a -> Parser b -> Parser [a]
pOneOrMoreWithSep pV pSep
  = pThen (:) pV (pZeroOrMore (pThen (const id) pSep pV))

#if __CLH_EXERCISE_1__ >= 16
pLit s = pSat (== s)

pSat :: (String -> Bool) -> Parser String
pSat pred ((_, tokVal) : toks)
  | pred tokVal = [(tokVal, toks)]
  | otherwise = []
pSat _ [] = []

#if __CLH_EXERCISE_1__ < 17
pVar = pSat isVal
  where
    isVal (c : _)
      | isAlpha c = True
      | otherwise = False
    isVal [] = False
#endif

#if __CLH_EXERCISE_1__ >= 17
keywords :: [String]
keywords = ["let", "letrec", "case", "in", "of", "Pack"]

pVar = pSat isVal
  where
    isVal cs@(c : _)
      | cs `elem` keywords = False
      | isAlpha c = True
      | otherwise = False
    isVal [] = False

#if __CLH_EXERCISE_1__ >= 18
pNum :: Parser Int
pNum = pSat isNumber `pApply` read
  where
    isNumber (c : _)
      | isDigit c = True
      | otherwise = False
    isNumber [] = False

#if __CLH_EXERCISE_1__ >= 19
pIfFail :: Parser a -> Parser a -> Parser a
pIfFail p1 p2 toks =
  case p1 toks of
    res@(_:_) -> res
    [] -> p2 toks

pZeroOrMore p = pOneOrMore p `pIfFail` pEmpty []

syntax = takeFirstParse . pProgram
  where
    takeFirstParse ((prog, []) : _) = prog
    takeFirstParse (_ : others) = takeFirstParse others
    takeFirstParse [] = error "Syntax error"

pProgram :: Parser CoreProgram
pProgram = pOneOrMoreWithSep pSc (pLit ";")

pSc :: Parser CoreScDefn
pSc = pThen4 mkSc pVar (pZeroOrMore pVar) (pLit "=") pExpr

mkSc :: Name -> [Name] -> a -> CoreExpr -> CoreScDefn
#if __CLH_EXERCISE_1__ >= 20
mkSc name vars _ expr = (name, vars, expr)
#else
mkSc = undefined
#endif

pExpr :: Parser CoreExpr
#if __CLH_EXERCISE_1__ >= 21
#if __CLH_EXERCISE_1__ < 23
pExpr
  = pLet recursive `pAlt`
    pLet nonRecursive `pAlt`
    pCase `pAlt`
    pLambda `pAlt`
    pAExpr
#endif

pLet :: IsRec -> Parser CoreExpr
pLet isRec = pThen4 (mkLet isRec) (pLit keyword) pDefns (pLit "in") pExpr
  where
    keyword
      | isRec = "letrec"
      | otherwise = "let"
mkLet :: IsRec -> a -> Assoc Name CoreExpr -> b -> CoreExpr -> CoreExpr
mkLet isRec _ defns _ = ELet isRec defns

pDefns :: Parser (Assoc Name CoreExpr)
pDefns = pOneOrMoreWithSep pDefn (pLit ";")
pDefn :: Parser (Name, CoreExpr)
pDefn = pThen3 mkDefn pVar (pLit "=") pExpr
mkDefn :: Name -> a -> CoreExpr -> (Name, CoreExpr)
mkDefn name _ expr = (name, expr)

pCase :: Parser CoreExpr
pCase = pThen4 mkCase (pLit "case") pExpr (pLit "of") pAlters
mkCase :: a -> CoreExpr -> b -> [CoreAlter] -> CoreExpr
mkCase _ expr _ = ECase expr
pAlters :: Parser [CoreAlter]
pAlters = pOneOrMoreWithSep pAlter (pLit ";")
pAlter :: Parser CoreAlter
pAlter = pThen3 mkAlter pPattern (pLit "->") pExpr
mkAlter :: (Int, [Name]) -> a -> CoreExpr -> CoreAlter
mkAlter (tag, vars) _ expr = (tag, vars, expr)
pPattern :: Parser (Int, [Name])
pPattern = pThen4 mkPattern (pLit "<") pNum (pLit ">") (pZeroOrMore pVar)
mkPattern :: a -> Int -> b -> [Name] -> (Int, [Name])
mkPattern _ tag _ vars = (tag, vars)

pLambda :: Parser CoreExpr
pLambda = pThen4 mkLambda (pLit "\\") (pOneOrMore pVar) (pLit ".") pExpr
mkLambda :: a -> [Name] -> b -> CoreExpr -> CoreExpr
mkLambda _ vars _ = ELam vars

pAExpr :: Parser CoreExpr
pAExpr
  = (pNum `pApply` ENum) `pAlt`
    (pVar `pApply` EVar) `pAlt`
    pConstr `pAlt`
    pThen3 ignoreParen (pLit "(") pExpr (pLit ")")
  where
    ignoreParen _ expr _ = expr

pConstr :: Parser CoreExpr
pConstr = pThen4 mkConstr (pLit "Pack") (pLit "{") pNums (pLit "}")
mkConstr :: a -> b -> (Int, Int) -> c -> CoreExpr
mkConstr _ _ (tag, arity) _ = EConstr tag arity
pNums :: Parser (Int, Int)
pNums = pThen3 mkNums pNum (pLit ",") pNum
mkNums :: Int -> a -> Int -> (Int, Int)
mkNums a _ b = (a, b)
#else
pExpr = undefined
#endif

#if __CLH_EXERCISE_1__ >= 23
#if __CLH_EXERCISE_1__ < 24
pExpr
  = (pOneOrMore pAExpr `pApply` mkApChain) `pAlt`
    pLet recursive `pAlt`
    pLet nonRecursive `pAlt`
    pCase `pAlt`
    pLambda `pAlt`
    pAExpr
#endif

mkApChain :: [CoreExpr] -> CoreExpr
mkApChain (expr:exprs) = foldl EAp expr exprs
mkApChain [] = error "Compiler Bug mkApChain"

#if __CLH_EXERCISE_1__ >= 24
data PartialExpr
  = NoOp
  | FoundOp Name CoreExpr

pExpr
  = pLet recursive `pAlt`
    pLet nonRecursive `pAlt`
    pCase `pAlt`
    pLambda `pAlt`
    pExpr1

pExpr1c :: Parser PartialExpr
pExpr1c
  = pThen FoundOp (pLit "|") pExpr1 `pAlt`
    pEmpty NoOp

pExpr1 :: Parser CoreExpr
pExpr1 = pThen assembleOp pExpr2 pExpr1c

pExpr2c :: Parser PartialExpr
pExpr2c
  = pThen FoundOp (pLit "&") pExpr2 `pAlt`
    pEmpty NoOp

pExpr2 :: Parser CoreExpr
pExpr2 = pThen assembleOp pExpr3 pExpr2c

pExpr3c :: Parser PartialExpr
pExpr3c
  = pThen FoundOp pRelOp pExpr3 `pAlt`
    pEmpty NoOp

relOps :: [Name]
relOps = ["<", "<=", "==", "~=", ">=", ">"]

pRelOp :: Parser Name
pRelOp = pSat (`elem` relOps)

pExpr3 :: Parser CoreExpr
pExpr3 = pThen assembleOp pExpr4 pExpr3c

pExpr4c :: Parser PartialExpr
pExpr4c
  = pThen FoundOp (pLit "+") pExpr4 `pAlt`
    pThen FoundOp (pLit "-") pExpr5 `pAlt`
    pEmpty NoOp

pExpr4 :: Parser CoreExpr
pExpr4 = pThen assembleOp pExpr5 pExpr4c

pExpr5c :: Parser PartialExpr
pExpr5c
  = pThen FoundOp (pLit "*") pExpr5 `pAlt`
    pThen FoundOp (pLit "/") pExpr6 `pAlt`
    pEmpty NoOp

pExpr5 :: Parser CoreExpr
pExpr5 = pThen assembleOp pExpr6 pExpr5c

pExpr6 :: Parser CoreExpr
pExpr6 = pOneOrMore pAExpr `pApply` mkApChain

assembleOp :: CoreExpr -> PartialExpr -> CoreExpr
assembleOp e1 NoOp = e1
assembleOp e1 (FoundOp op e2) = EAp (EAp (EVar op) e1) e2
#endif
#endif
#endif
#endif
#endif
#endif
#endif
#endif
#endif
#endif
#else
syntax = undefined
#endif
