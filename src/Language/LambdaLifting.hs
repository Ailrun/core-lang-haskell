{-# LANGUAGE CPP #-}
module Language.LambdaLifting
  ( lambdaRun
  , lambdaLift
  , runS
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

rename = snd . mapAccumL renameSc initialNameSupply
  where
    renameSc ns (scName, args, rhs)
      = (ns2, (scName, args', rhs'))
      where
        (ns1, args', env) = newNames ns args
        (ns2, rhs') = renameE env ns1 rhs

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
    collectOneSc (scName, args, ELam args' body)
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

    collectScsD scs (name, ELam rhsArgs rhsBody) = ((scs ++) *** (,) name . ELam rhsArgs) (collectScsE rhsBody)
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
#endif
#endif
#endif
