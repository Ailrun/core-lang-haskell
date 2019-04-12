{-# LANGUAGE CPP #-}
module Language.LambdaLifting
  ( lambdaRun
  , lambdaLift
  , runS
#if __CLH_EXERCISE_6__ >= 7
  , lambdaRunJ
  , lambdaLiftJ
  , runJ
#if __CLH_EXERCISE_6__ >= 9
  , lambdaRunF
  , fullyLazyLift
  , runF
#endif
#endif
  )
where

import Control.Arrow
import Data.List
import Language.Parser
import Language.PrettyPrinter
import Language.Types
import Util

lambdaRun = putStrLn . runS

lambdaLift :: CoreProgram -> CoreProgram

freeVars :: CoreProgram -> AnnProgram Name (Set Name)

abstract :: AnnProgram Name (Set Name) -> CoreProgram

rename :: CoreProgram -> CoreProgram

collectScs :: CoreProgram -> CoreProgram

lambdaLift = collectScs . rename . abstract . freeVars

runS = prettyPrint . lambdaLift . parse

freeVars program
  = [ (name, args, freeVarsE (setFromList args) body)
    | (name, args, body) <- program
    ]

freeVarsE :: Set Name -> CoreExpr -> AnnExpr Name (Set Name)
freeVarsE _ (ENum k) = (setEmpty, ANum k)
freeVarsE lv (EVar v)
  | setElementOf v lv = (setSingleton v, AVar v)
  | otherwise = (setEmpty, AVar v)
freeVarsE lv (EAp e1 e2)
  = (setUnion (freeVarsOf e1') (freeVarsOf e2'), AAp e1' e2')
  where
    e1' = freeVarsE lv e1
    e2' = freeVarsE lv e2
freeVarsE lv (ELam args body)
  = (setSubtraction (freeVarsOf body') (setFromList args), ALam args body')
  where
    body' = freeVarsE newLv body
    newLv = setUnion lv (setFromList args)
freeVarsE lv (ELet isRec defns body)
  = (setUnion defnsFree bodyFree, ALet isRec defns' body')
  where
    binders = bindersOf defns
    binderSet = setFromList binders
    bodyLv = setUnion lv binderSet
    rhsLv
      | isRec = bodyLv
      | otherwise = lv

    rhss' = map (freeVarsE rhsLv) (rhssOf defns)
    defns' = zip binders rhss'
    freeInRhss = setUnionList (map freeVarsOf rhss')
    defnsFree
      | isRec = setSubtraction freeInRhss binderSet
      | otherwise = freeInRhss
    body' = freeVarsE bodyLv body
    bodyFree = setSubtraction (freeVarsOf body') binderSet
freeVarsE lv (ECase e alts) = freeVarsCase lv e alts
freeVarsE _ (EConstr t a) = (setEmpty, AConstr t a)

freeVarsCase :: Set Name -> CoreExpr -> [CoreAlter] -> AnnExpr Name (Set Name)
#if __CLH_EXERCISE_6__ < 4
freeVarsCase lv e alts = error "freeVarsCase: not yet implemented"
#endif

freeVarsOf :: AnnExpr Name (Set Name) -> Set Name
freeVarsOf (fvs, expr) = fvs

freeVarsOfAlter :: AnnAlter Name (Set Name) -> Set Name
freeVarsOfAlter (tag, args, rhs)
  = setSubtraction (freeVarsOf rhs) (setFromList args)

abstract program
  = [ (scName, args, abstractE rhs)
    | (scName, args, rhs) <- program
    ]

abstractE :: AnnExpr Name (Set Name) -> CoreExpr
abstractE (_, ANum n) = ENum n
abstractE (_, AVar v) = EVar v
abstractE (_, AAp e1 e2) = EAp (abstractE e1) (abstractE e2)
abstractE (_, ALet isRec defns body)
  = ELet isRec (map (second abstractE) defns) (abstractE body)
abstractE (free, ALam args body)
  = foldl EAp sc (map EVar fvs)
  where
    fvs = setToList free
    sc = ELet nonRecursive [("sc", scRhs)] (EVar "sc")
    scRhs = ELam (fvs ++ args) (abstractE body)
abstractE (_, AConstr t a) = EConstr t a
abstractE (free, ACase e alts) = abstractCase free e alts

abstractCase :: Set Name -> AnnExpr Name (Set Name) -> [AnnAlter Name (Set Name)] -> CoreExpr
#if __CLH_EXERCISE_6__ < 4
abstractCase free e alts = error "abstractCase: not yet implemented"
#endif

#if __CLH_EXERCISE_6__ < 9
rename = snd . mapAccumL renameSc initialNameSupply
  where
    renameSc ns (scName, args, rhs)
      = (ns2, (scName, args', rhs'))
      where
        (ns1, args', env) = newNames ns args
        (ns2, rhs') = renameE env ns1 rhs
#endif

newNames :: NameSupply -> [Name] -> (NameSupply, [Name], Assoc Name Name)
newNames ns oldNames
  = (ns', newNames, env)
  where
    (ns', newNames) = getNames ns oldNames
    env = zip oldNames newNames

renameE :: Assoc Name Name -> NameSupply -> CoreExpr -> (NameSupply, CoreExpr)
renameE _ ns (ENum n) = (ns, ENum n)
renameE env ns (EVar v) = (ns, EVar (aLookup env v v))
renameE env ns (EAp e1 e2)
  = (ns2, EAp e1' e2')
  where
    (ns1, e1') = renameE env ns e1
    (ns2, e2') = renameE env ns1 e2
renameE env ns (ELam args body)
  = (ns2, ELam args' body')
  where
    (ns1, args', env') = newNames ns args
    (ns2, body') = renameE (env' ++ env) ns1 body
renameE env ns (ELet isRec defns body)
  = (ns3, ELet isRec (zip binders' rhss') body')
  where
    (ns1, body') = renameE bodyEnv ns body
    binders = bindersOf defns
    (ns2, binders', env') = newNames ns1 binders
    bodyEnv = env' ++ env
    (ns3, rhss') = mapAccumL (renameE rhsEnv) ns2 (rhssOf defns)
    rhsEnv
      | isRec = bodyEnv
      | otherwise = env
renameE env ns (EConstr t a) = (ns, EConstr t a)
renameE env ns (ECase e alts) = renameCase env ns e alts

renameCase :: Assoc Name Name -> NameSupply -> CoreExpr -> [CoreAlter] -> (NameSupply, CoreExpr)
#if __CLH_EXERCISE_6__ < 4
renameCase env ns e alts = error "renameCase: not yet implemented"
#endif

#if __CLH_EXERCISE_6__ < 5
collectScs = concatMap collectOneSc
  where
    collectOneSc (scName, args, rhs)
      = (scName, args, rhs') : scs
      where
        (scs, rhs') = collectScsE rhs
#endif

collectScsE :: CoreExpr -> ([CoreScDefn], CoreExpr)
#if __CLH_EXERCISE_6__ < 6
collectScsE (ENum n) = ([], ENum n)
collectScsE (EVar v) = ([], EVar v)
collectScsE (EAp e1 e2) = (scs1 ++ scs2, EAp e1' e2')
  where
    (scs1, e1') = collectScsE e1
    (scs2, e2') = collectScsE e2
collectScsE (ELam args body) = second (ELam args) (collectScsE body)
collectScsE (ELet isRec defns body)
  = (rhssScs ++ bodyScs ++ localScs, mkELet isRec nonScs' body')
  where
    (rhssScs, defns') = mapAccumL collectScsD [] defns

    scs' = filter (isELam . snd) defns'
    nonScs' = filter (not . isELam . snd) defns'
    localScs
      = [ (name, args, body)
        | (name, ELam args body) <- scs'
        ]

    (bodyScs, body') = collectScsE body

    collectScsD scs (name, rhs) = ((scs ++) *** (,) name) (collectScsE rhs)
collectScsE (EConstr t a) = ([], EConstr t a)
collectScsE (ECase e alts)
  = (scsE ++ scsAlts, ECase e' alts')
  where
    (scsE, e') = collectScsE e
    (scsAlts, alts') = mapAccumL collectScsAlt [] alts

    collectScsAlt scs (tag, args, rhs)
      = ((scs ++) *** (,,) tag args) (collectScsE rhs)
#endif

isELam :: Expr a -> Bool
isELam (ELam _ _) = True
isELam _ = False

mkELet :: IsRec -> Assoc a (Expr a) -> Expr a -> Expr a
#if __CLH_EXERCISE_6__ < 3
mkELet = ELet
#endif

#if __CLH_EXERCISE_6__ >= 3
mkELet _ [] body = body
mkELet isRec defns body = ELet isRec defns body

#if __CLH_EXERCISE_6__ >= 4
freeVarsCase lv e alts
  = (setUnion eFree altsFree, ACase e' alts')
  where
    e' = freeVarsE lv e
    alts' = map freeVarsAlter alts

    eFree = freeVarsOf e'
    altsFree = setUnionList (map freeVarsOfAlter alts')

    freeVarsAlter (tag, args, rhs) = (tag, args, freeVarsE rhsLv rhs)
      where
        rhsLv = setUnion argSet lv
        argSet = setFromList args

abstractCase free e alts = ECase (abstractE e) (map abstractAlter alts)

abstractAlter :: AnnAlter Name (Set Name) -> CoreAlter
abstractAlter (tag, args, rhs) = (tag, args, abstractE rhs)

renameCase env ns e alts = (ns2, ECase e' alts')
  where
    (ns1, e') = renameE env ns e
    (ns2, alts') = mapAccumL (renameAlter env) ns1 alts

renameAlter :: Assoc Name Name -> NameSupply -> CoreAlter -> (NameSupply, CoreAlter)
renameAlter env ns (tag, args, rhs) = (ns2, (tag, args', rhs'))
  where
    (ns1, args', env') = newNames ns args
    (ns2, rhs') = renameE (env' ++ env) ns1 rhs

#if __CLH_EXERCISE_6__ >= 5
collectScs = concatMap collectOneSc
  where
    collectOneSc (scName, args, ELet isRec [(name1, ELam args' body)] (EVar name2))
      | not isRec && name1 == name2
      = (scName, args ++ args', body') : scs
      where
        (scs, body') = collectScsE body
    collectOneSc (scName, args, rhs)
      = (scName, args, rhs') : scs
      where
        (scs, rhs') = collectScsE rhs

#if __CLH_EXERCISE_6__ >= 6
collectScsE (ENum n) = ([], ENum n)
collectScsE (EVar v) = ([], EVar v)
collectScsE (EAp e1 e2) = (scs1 ++ scs2, EAp e1' e2')
  where
    (scs1, e1') = collectScsE e1
    (scs2, e2') = collectScsE e2
collectScsE (ELam args body) = second (ELam args) (collectScsE body)
collectScsE (ELet isRec defns body)
  = (rhssScs ++ bodyScs ++ localScs, mkELet isRec nonScs' body')
  where
    (rhssScs, defns') = mapAccumL collectScsD [] defns

    scs' = filter (isELam . snd) defns'
    nonScs' = filter (not . isELam . snd) defns'
    localScs
      = [ (name, args, body)
        | (name, ELam args body) <- scs'
        ]

    (bodyScs, body') = collectScsE body

    collectScsD scs (name, ELet isRec [(name1, ELam rhsArgs rhsBody)] (EVar name2))
      | not isRec && name1 == name2
      = ((scs ++) *** (,) name . ELam rhsArgs) (collectScsE rhsBody)
    collectScsD scs (name, rhs) = ((scs ++) *** (,) name) (collectScsE rhs)
collectScsE (EConstr t a) = ([], EConstr t a)
collectScsE (ECase e alts)
  = (scsE ++ scsAlts, ECase e' alts')
  where
    (scsE, e') = collectScsE e
    (scsAlts, alts') = mapAccumL collectScsAlt [] alts

    collectScsAlt scs (tag, args, rhs)
      = ((scs ++) *** (,,) tag args) (collectScsE rhs)

#if __CLH_EXERCISE_6__ >= 7
abstractJ :: AnnProgram Name (Set Name) -> CoreProgram

lambdaRunJ = putStrLn . runJ

lambdaLiftJ :: CoreProgram -> CoreProgram
lambdaLiftJ = collectScs . abstractJ . freeVars . rename

runJ = prettyPrint . lambdaLiftJ . parse

abstractJE :: Assoc Name [Name] -> AnnExpr Name (Set Name) -> CoreExpr

abstractJ program
  = [ (name, args, abstractJE [] rhs)
    | (name, args, rhs) <- program
    ]

abstractJE env (_, ANum n) = ENum n
abstractJE env (_, AConstr t a) = EConstr t a
abstractJE env (_, AAp e1 e2) = EAp (abstractJE env e1) (abstractJE env e2)
abstractJE env (_, AVar g) = foldl EAp (EVar g) (map EVar (aLookup env g []))
abstractJE env (free, ALam args body)
  = foldl EAp sc (map EVar fvList)
  where
    fvList = actualFreeList env free
    sc = ELet nonRecursive [("sc", scRhs)] (EVar "sc")
    scRhs = ELam (fvList ++ args) (abstractJE env body)
abstractJE env (_, ALet isRec defns body) = ELet isRec (funDefns' ++ varDefns') body'
  where
    (funDefns, varDefns) = partition (isALam . snd) defns

    funNames = bindersOf funDefns
    freeInFuns = setSubtraction (setUnionList (map (freeVarsOf . snd) funDefns)) (setFromList funNames)
    varsToAbstract = actualFreeList env freeInFuns

    bodyEnv = map (flip (,) varsToAbstract) funNames ++ env
    rhsEnv
      | isRec = bodyEnv
      | otherwise = env

    funDefns'
      = [ (name, ELam (varsToAbstract ++ args) (abstractJE rhsEnv body))
        | (name, (_, ALam args body)) <- funDefns
        ]
    varDefns' = map (second (abstractJE rhsEnv)) varDefns
    body' = abstractJE bodyEnv body
abstractJE env (_, ACase e alts) = ECase e' alts'
  where
    e' = abstractJE env e
    alts'
      = [ (altTag, altArgs, abstractJE env altBody)
        | (altTag, altArgs, altBody) <- alts
        ]

actualFreeList :: Assoc Name [Name] -> Set Name -> [Name]
actualFreeList env free
  = setToList (setUnionList [ setFromList (aLookup env name [name])
                            | name <- setToList free
                            ])

isALam :: AnnExpr a b -> Bool
isALam (_, ALam _ _) = True
isALam _ = False

#if __CLH_EXERCISE_6__ >= 8
-- | It's not yet done

#if __CLH_EXERCISE_6__ >= 9
lambdaRunF = putStrLn . runF

separateLams :: CoreProgram -> CoreProgram

type Level = Int

addLevels :: CoreProgram -> AnnProgram (Name, Level) Level

identifyMFEs :: AnnProgram (Name, Level) Level -> Program (Name, Level)

float :: Program (Name, Level) -> CoreProgram

fullyLazyLift :: CoreProgram -> CoreProgram
fullyLazyLift = float . renameL . identifyMFEs . addLevels . separateLams

runF :: String -> String
runF = prettyPrint . lambdaLift . fullyLazyLift . parse

separateLamsE :: CoreExpr -> CoreExpr

separateLams program
  = [ (name, [], mkSepLams args (separateLamsE rhs))
    | (name, args, rhs) <- program
    ]

separateLamsE e@(ENum _) = e
separateLamsE e@(EVar _) = e
separateLamsE (EAp e1 e2) = EAp (separateLamsE e1) (separateLamsE e2)
separateLamsE (ELam args body) = mkSepLams args (separateLamsE body)
separateLamsE (ELet isRec defns body) = ELet isRec defns' body'
  where
    defns' = map (second separateLamsE) defns
    body' = separateLamsE body
separateLamsE e@(EConstr _ _) = e
separateLamsE (ECase e alts) = ECase e' alts'
  where
    e' = separateLamsE e
    alts'
      = [ (name, args, separateLamsE rhs)
        | (name, args, rhs) <- alts
        ]

mkSepLams :: [Name] -> CoreExpr -> CoreExpr
mkSepLams = flip (foldr mkSepLam)
  where
    mkSepLam arg body = ELam [arg] body

freeToLevel :: AnnProgram Name (Set Name) -> AnnProgram (Name, Level) Level

addLevels = freeToLevel . freeVars

freeToLevelSc :: AnnScDefn Name (Set Name) -> AnnScDefn (Name, Level) Level
freeToLevelE :: Level -> Assoc Name Level -> AnnExpr Name (Set Name) -> AnnExpr (Name, Level) Level

freeToLevel = map freeToLevelSc

freeToLevelSc (scName, [], rhs) = (scName, [], freeToLevelE 0 [] rhs)

freeToLevelE _ _ (_, ANum n) = (0, ANum n)
freeToLevelE _ env (_, AVar v) = (aLookup env v 0, AVar v)
freeToLevelE level env (_, AAp e1 e2) = (max (levelOf e1') (levelOf e2'), AAp e1' e2')
  where
    e1' = freeToLevelE level env e1
    e2' = freeToLevelE level env e2
freeToLevelE level env (free, ALam [arg] body) = (freeSetToLevel env free, ALam [arg'] body')
  where
    arg' = (arg, level + 1)
    body' = freeToLevelE (level + 1) (arg' : env) body
freeToLevelE level env (_, ALet isRec defns body) = (levelOf body', ALet isRec defns' body')
  where
    binders = bindersOf defns
    rhss = rhssOf defns

    binder' = map (flip (,) maxRhsLevel) binders
    rhss' = map (freeToLevelE level rhsEnv) rhss
    defns' = zip binder' rhss'
    body' = freeToLevelE level bodyEnv body

    freeInRhss = setUnionList (map fst rhss)
    maxRhsLevel = freeSetToLevel levelRhsEnv freeInRhss

    bodyEnv = binder' ++ env
    rhsEnv
      | isRec = bodyEnv
      | otherwise = env
    levelRhsEnv
      | isRec = map (flip (,) 0) binders ++ env
      | otherwise = env
freeToLevelE level env (free, ACase e alts) = freeToLevelCase level env free e alts

freeToLevelCase :: Level -> Assoc Name Level -> Set Name -> AnnExpr Name (Set Name) -> [AnnAlter Name (Set Name)] -> AnnExpr (Name, Level) Level
#if __CLH_EXERCISE_6__ < 11
freeToLevelCase level env free e alts = error "freeToLevelCase: not yet written"
#endif

levelOf :: AnnExpr (Name, Level) Level -> Level
levelOf (l, _) = l

freeSetToLevel :: Assoc Name Level -> Set Name -> Level
freeSetToLevel env = foldl max 0 . map (flip (aLookup env) 0) . setToList

identifyMFEsE :: Level -> AnnExpr (Name, Level) Level -> Expr (Name, Level)

identifyMFEs program
  = [ (scName, [], identifyMFEsE 0 rhs)
    | (scName, [], rhs) <- program
    ]

notMFECandidate :: AnnExpr' (Name, Level) Level -> Bool
notMFECandidate (ANum _) = True
notMFECandidate (AVar _) = True
notMFECandidate (AConstr _ _) = True
notMFECandidate _ = False

identifyMFEsE' :: Level -> AnnExpr' (Name, Level) Level -> Expr (Name, Level)

identifyMFEsE ctx (level, e)
  | level == ctx || notMFECandidate e = e'
  | otherwise = transformMFE level e'
  where
    e' = identifyMFEsE' level e

transformMFE :: Level -> Expr (Name, Level) -> Expr (Name, Level)
transformMFE level e = ELet nonRecursive [(("v", level), e)] (EVar "v")

identifyMFEsE' _ (ANum n) = ENum n
identifyMFEsE' _ (AVar v) = EVar v
identifyMFEsE' level (AAp e1 e2) = EAp (identifyMFEsE level e1) (identifyMFEsE level e2)
identifyMFEsE' _ (ALam [arg] body) = ELam [arg] (identifyMFEsE (snd arg) body)
identifyMFEsE' level (ALet isRec defns body) = ELet isRec defns' body'
  where
    body' = identifyMFEsE level body
    defns'
      = [ (binding, identifyMFEsE (snd binding) rhs)
        | (binding, rhs) <- defns
        ]
identifyMFEsE' _ (AConstr t a) = EConstr t a
identifyMFEsE' level (ACase e alts) = identifyMFEsCase level e alts

identifyMFEsCase :: Level -> AnnExpr (Name, Level) Level -> [AnnAlter (Name, Level) Level] -> Expr (Name, Level)
#if __CLH_EXERCISE_6__ < 11
identifyMFEsCase level e alts = error "identifyMFEsCase: not yet written"
#endif

renameGen :: (NameSupply -> [a] -> (NameSupply, [a], Assoc Name Name)) -> Program a -> Program a

rename = renameGen newNames

#if __CLH_EXERCISE_6__ < 10
renameL :: Program (Name, Level) -> Program (Name, Level)
#endif
renameL = renameGen newNamesL

#if __CLH_EXERCISE_6__ < 10
newNamesL :: NameSupply -> [(Name, Level)] -> (NameSupply, [(Name, Level)], Assoc Name Name)
#endif
newNamesL ns binders
  = (ns', binders', env)
  where
    names = map fst binders
    levels = map snd binders
    (ns', names') = getNames ns names
    binders' = zip names' levels
    env = zip names names'

renameGenE :: (NameSupply -> [a] -> (NameSupply, [a], Assoc Name Name)) -> Assoc Name Name -> NameSupply -> Expr a -> (NameSupply, Expr a)

renameGen newBinder = snd . mapAccumL renameSc initialNameSupply
  where
    renameSc ns (scName, args, rhs)
      = (ns2, (scName, args', rhs'))
      where
        (ns1, args', env) = newBinder ns args
        (ns2, rhs') = renameGenE newBinder env ns1 rhs

renameGenE newBinder _ ns (ENum n) = (ns, ENum n)
renameGenE newBinder env ns (EVar v) = (ns, EVar (aLookup env v v))
renameGenE newBinder env ns (EAp e1 e2)
  = (ns2, EAp e1' e2')
  where
    (ns1, e1') = renameGenE newBinder env ns e1
    (ns2, e2') = renameGenE newBinder env ns1 e2
renameGenE newBinder env ns (ELam args body)
  = (ns2, ELam args' body')
  where
    (ns1, args', env') = newBinder ns args
    (ns2, body') = renameGenE newBinder (env' ++ env) ns1 body
renameGenE newBinder env ns (ELet isRec defns body)
  = (ns3, ELet isRec (zip binders' rhss') body')
  where
    (ns1, body') = renameGenE newBinder bodyEnv ns body
    binders = bindersOf defns
    (ns2, binders', env') = newBinder ns1 binders
    bodyEnv = env' ++ env
    (ns3, rhss') = mapAccumL (renameGenE newBinder rhsEnv) ns2 (rhssOf defns)
    rhsEnv
      | isRec = bodyEnv
      | otherwise = env
renameGenE newBinder env ns (EConstr t a) = (ns, EConstr t a)
renameGenE newBinder env ns (ECase e alts) = renameGenCase newBinder env ns e alts

renameGenCase :: (NameSupply -> [a] -> (NameSupply, [a], Assoc Name Name)) -> Assoc Name Name -> NameSupply -> Expr a -> [Alter a] -> (NameSupply, Expr a)
renameGenCase newBinder env ns e alts = (ns2, ECase e' alts')
  where
    (ns1, e') = renameGenE newBinder env ns e
    (ns2, alts') = mapAccumL (renameGenAlter newBinder env) ns1 alts

renameGenAlter :: (NameSupply -> [a] -> (NameSupply, [a], Assoc Name Name)) -> Assoc Name Name -> NameSupply -> Alter a -> (NameSupply, Alter a)
renameGenAlter newBinder env ns (tag, args, rhs) = (ns2, (tag, args', rhs'))
  where
    (ns1, args', env') = newBinder ns args
    (ns2, rhs') = renameGenE newBinder (env' ++ env) ns1 rhs

#if __CLH_EXERCISE_6__ >= 10
renameL :: Program (Name, a) -> Program (Name, a)
newNamesL :: NameSupply -> [(Name, a)] -> (NameSupply, [(Name, a)], Assoc Name Name)
#if __CLH_EXERCISE_6__ >= 11
floatSc :: ScDefn (Name, Level) -> [CoreScDefn]

float = concat . map floatSc

floatE :: Expr (Name, Level) -> (FloatedDefns, Expr Name)

floatSc (name, [], rhs)
  = [(name, [], rhs')] ++ concat (map toScs fds)
  where
    (fds, rhs') = floatE rhs
    toScs (level, isRec, defns) = map makeSc defns
    makeSc (name, rhs) = (name, [], rhs)

type FloatedDefns = [(Level, IsRec, [(Name, Expr Name)])]

floatE (ENum n) = ([], ENum n)
floatE (EVar v) = ([], EVar v)
floatE (EAp e1 e2) = (fd1 ++ fd2, EAp e1' e2')
  where
    (fd1, e1') = floatE e1
    (fd2, e2') = floatE e2
floatE (ELam [arg] body) = (fdOuter, ELam [arg'] (install fdThisLevel body'))
  where
    (arg', thisLevel) = arg
    (fdBody, body') = floatE body
    (fdOuter, fdThisLevel) = partitionFloats thisLevel fdBody
floatE (ELet isRec defns body) = (rhsd ++ [thisGroup] ++ bodyd, body')
  where
    (bodyd, body') = floatE body
    (rhsd, defns') = mapAccumL floatDefn [] defns
    thisGroup = (snd . head . bindersOf $ defns, isRec, defns')
floatE (ECase e alts) = floatCase e alts

floatCase :: Expr (Name, Level) -> [Alter (Name, Level)] -> (FloatedDefns, Expr Name)
#if __CLH_EXERCISE_6__ < 11
floatCase e alts = error "floatCase not yet written"
#endif

floatDefn :: FloatedDefns -> ((Name, Level), Expr (Name, Level)) -> (FloatedDefns, (Name, Expr Name))
floatDefn ds ((name, level), rhs)
  = (rhsd ++ ds, (name, rhs'))
  where
    (rhsd, rhs') = floatE rhs

install :: FloatedDefns -> Expr Name -> Expr Name
install defnGroups e
  = foldr installGroup e defnGroups
  where
    installGroup (level, isRec, defns) e = ELet isRec defns e

partitionFloats :: Level -> FloatedDefns -> (FloatedDefns, FloatedDefns)
partitionFloats thisLevel fds = partition isOuterLevel fds
  where
    isOuterLevel (level, _, _) = level < thisLevel

#if __CLH_EXERCISE_6__ >= 11
freeToLevelCase level env free e alts = (freeSetToLevel env free, ACase e' alts')
  where
    e' = freeToLevelE level env e
    alts' = map freeToLevelAlt alts

    freeToLevelAlt (tag, args, rhs)
      = (tag, args', freeToLevelE (level + 1) env' rhs)
      where
        env' = args' ++ env
        args' = map (flip (,) (level + 1)) args

identifyMFEsCase level e alts = ECase (identifyMFEsE level e) (map identifyMFEsAlt alts)
  where
    identifyMFEsAlt (tag, [], e) = (tag, [], identifyMFEsE level e)
    identifyMFEsAlt (tag, args, e) = (tag, args, identifyMFEsE argLevel e)
      where
        (_, argLevel) = head args

floatCase e alts = (eds ++ altds, ECase e' alts')
  where
    (eds, e') = floatE e
    (altds, alts') = first concat . unzip . map floatAlter $ alts

    floatAlter (tag, [], rhs) = second ((,,) tag []) (floatE rhs)
    floatAlter (tag, args, rhs) = (fdOuter, (tag, args', install fdThisLevel rhs'))
      where
        args' = map fst args
        (_, thisLevel) = head args
        (fdRhs, rhs') = floatE rhs
        (fdOuter, fdThisLevel) = partitionFloats thisLevel fdRhs
#endif
#else
float = undefined
#endif
#endif
#endif
#endif
#endif
#endif
#endif
#endif
#endif
