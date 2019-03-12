module Language.Parser where

import Data.Char
import Language.Types
import Util

-- |
-- Before exercise 1.11
{-
clex :: String -> [Token]
-}
syntax :: [Token] -> CoreProgram

parse :: String -> CoreProgram
parse = syntax . clex 0

-- |
-- Before exercise 1.11
{-
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
-- |
-- Following pattern is exercise 1.9
clex ('|' : '|' : cs) = clex restCs
  where
    restCs = dropWhile (`notElem` "\r\n") cs
-- |
-- Following pattern is a part of exercise 1.10
clex (c0 : c1 : cs) | op `elem` twoCharOps = op : clex cs
  where
    op = [c0, c1]
clex (c : cs) = [c] : clex cs
clex [] = []
-}

isIdChar c = isAlpha c || isDigit c || c == '_'

-- |
-- Following 'twoCharOps' is a part of exercise 1.10
twoCharOps :: [String]
twoCharOps = ["==", "~=", ">=", "<=", "->"]

-- |
-- Following 'Token' and clex are exercise 1.11
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
-- |
-- Before 'pSat'
{-
pLit s ((_, tokVal) : toks)
  | s == tokVal = [(s, toks)]
  | otherwise = []
pLit _ [] = []
-}

pVar :: Parser String
-- |
-- Before 'pSat'
{-
pVar ((_, (c : cs)) : toks)
  | isAlpha c = [(c : cs, toks)]
  | otherwise = []
pVar ((_, []) : _) = []
pVar [] = []
-}

pAlt :: Parser a -> Parser a -> Parser a
pAlt p1 p2 toks = p1 toks ++ p2 toks

pThen :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
pThen combine p1 p2 toks
  = [ (combine v1 v2, toks2)
    | (v1, toks1) <- p1 toks
    , (v2, toks2) <- p2 toks1
    ]

-- |
-- Following 'pThen3' and 'pThen4' are exercise 1.12
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


pZeroOrMore :: Parser a -> Parser [a]
-- |
-- This one shows bad performance
-- since it always produces empty result even when
-- first parser succeeded.
-- Before exercise 1.19
{-
pZeroOrMore p = pOneOrMore p `pAlt` pEmpty []
-}

-- |
-- Following 'pEmpty' and 'pOneOrMore' are exercise 1.13
pEmpty :: a -> Parser a
pEmpty v toks = [(v, toks)]

pOneOrMore :: Parser a -> Parser [a]
pOneOrMore p = pThen (:) p (pZeroOrMore p)

-- |
-- Following 'pApply' is exercise 1.14
pApply :: Parser a -> (a -> b) -> Parser b
pApply p f toks
  = [ (f v', toks')
    | (v', toks') <- p toks
    ]

-- |
-- Following 'pApply' is exercise 1.15
pOneOrMoreWithSep :: Parser a -> Parser b -> Parser [a]
pOneOrMoreWithSep pV pSep
  = pThen (:) pV (pZeroOrMore (pThen (const id) pSep pV))

pLit s = pSat (== s)

-- |
-- Following 'pSat' and 'pVar' are exercise 1.16
pSat :: (String -> Bool) -> Parser String
pSat pred ((_, tokVal) : toks)
  | pred tokVal = [(tokVal, toks)]
  | otherwise = []
pSat _ [] = []

-- |
-- Before exercise 1.17
{-
pVar = pSat isVal
  where
    isVal (c : _)
      | isAlpha c = True
      | otherwise = False
    isVal [] = False
-}

-- |
-- Following 'keywords' and 'pVar' are exercise 1.17
keywords :: [String]
keywords = ["let", "letrec", "case", "in", "of", "Pack"]

pVar = pSat isVal
  where
    isVal cs@(c : _)
      | cs `elem` keywords = False
      | isAlpha c = True
      | otherwise = False
    isVal [] = False

-- |
-- Following 'pNum' is exercise 1.18
pNum :: Parser Int
pNum = pSat isNumber `pApply` read
  where
    isNumber (c : _)
      | isDigit c = True
      | otherwise = False
    isNumber [] = False

-- |
-- Following 'pIfFail' and 'pZeroOrMore' are exercise 1.19
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

-- |
-- Following 'mkSc' is exercise 1.20
mkSc :: Name -> [Name] -> a -> CoreExpr -> CoreScDefn
mkSc name vars _ expr = (name, vars, expr)

-- |
-- Following
-- 'pExpr', 'pLet', 'mkLet', 'pDefns', 'pDefn', 'mkDefn',
-- 'pCase', 'mkCase', 'pAlters', 'pAlter', 'mkAlter',
-- 'pPattern', 'mkPattern', 'pLambda', 'mkLambda',
-- 'pAExpr', 'pConstr', 'mkConstr', 'pNums', 'mkNums'
-- are exercise 1.21
pExpr :: Parser CoreExpr
-- |
-- Before exercise 1.23
{-
pExpr
  = pLet recursive `pAlt`
    pLet nonRecursive `pAlt`
    pCase `pAlt`
    pLambda `pAlt`
    pAExpr
-}

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

-- |
-- Following 'pExpr' and 'mkApChain' is exercise 1.23
-- |
-- Before exercise 1.24
{-
pExpr
  = (pOneOrMore pAExpr `pApply` mkApChain) `pAlt`
    pLet recursive `pAlt`
    pLet nonRecursive `pAlt`
    pCase `pAlt`
    pLambda `pAlt`
    pAExpr
-}

mkApChain :: [CoreExpr] -> CoreExpr
mkApChain (expr:exprs) = foldl EAp expr exprs
mkApChain [] = error "Compiler Bug mkApChain"

-- |
-- Following 'PartialExpr', 'pExpr',
-- 'pExpr1', 'pExpr1c', 'pExpr2', 'pExpr2c',
-- 'pExpr3', 'pExpr3c', 'relOps', 'pRelOp',
-- 'pExpr4', 'pExpr4c', 'pExpr5', 'pExpr5c' and
-- 'pExpr6', 'assembleOp' are exercise 1.24
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
