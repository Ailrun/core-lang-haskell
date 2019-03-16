{-# LANGUAGE CPP #-}
module Language.TiMachine where

import Data.ISeq
import Data.List
#if __CLH_EXERCISE_2__ >= 7
import Data.StatHeap
#endif
import Language.Parser
import Language.Prelude
import Language.Types
import Util

run :: String -> String
compile :: CoreProgram -> TiState
eval :: TiState -> [TiState]
showResults :: [TiState] -> String

run = showResults . eval . compile . parse

#if __CLH_EXERCISE_2__ < 26
type TiState = (TiStack, TiDump, TiHeap, TiGlobals, TiStats)
#endif

type TiStack = [Addr]

#if __CLH_EXERCISE_2__ < 16
data TiDump = DummyTiDump
initialTiDump = DummyTiDump
#endif

#if __CLH_EXERCISE_2__ < 7
type TiHeap = Heap Node
#endif

#if __CLH_EXERCISE_2__ < 13
data Node
  = NAp Addr Addr
  | NSc Name [Name] CoreExpr
  | NNum Int
#endif

type TiGlobals = Assoc Name Addr

tiStatInitial :: TiStats
tiStatIncSteps :: TiStats -> TiStats
tiStatGetSteps :: TiStats -> Int

#if __CLH_EXERCISE_2__ < 7
type TiStats = Int

tiStatInitial = 0
tiStatIncSteps = (+ 1)
tiStatGetSteps s = s
#endif

applyToStats :: (TiStats -> TiStats) -> TiState -> TiState
#if __CLH_EXERCISE_2__ < 26
applyToStats statFun (stack, dump, heap, scDefs, stats)
  = (stack, dump, heap, scDefs, statFun stats)

compile program
  = (initialStack, initialTiDump, initialHeap, globals, tiStatInitial)
  where
    scDefs = program ++ preludeDefs ++ extraPreludeDefs

    (initialHeap, globals) = buildInitialHeap scDefs
    initialStack = [addressOfMain]
    addressOfMain = aLookup globals "main" (error "main is not defined")
#endif

extraPreludeDefs :: CoreProgram
#if __CLH_EXERCISE_2__ < 20
extraPreludeDefs = []
#endif

buildInitialHeap :: [CoreScDefn] -> (TiHeap, TiGlobals)
#if __CLH_EXERCISE_2__ < 7
buildInitialHeap = mapAccumL allocateSc hInitial
#endif

allocateSc :: TiHeap -> CoreScDefn -> (TiHeap, (Name, Addr))
#if __CLH_EXERCISE_2__ < 7
allocateSc heap (name, args, body)
  = (heap', (name, addr))
  where
    (heap', addr) = hAlloc heap (NSc name args body)
#endif

eval state
  = state : restStates
  where
    restStates
      | tiFinal state = []
      | otherwise = eval nextState
    nextState = doAdmin (step state)

doAdmin :: TiState -> TiState
#if __CLH_EXERCISE_2__ < 7
doAdmin = applyToStats tiStatIncSteps
#endif

tiFinal :: TiState -> Bool
#if __CLH_EXERCISE_2__ < 7
tiFinal ([soleAddr], _, heap, _, _) = isDataNode (hLookup heap soleAddr)
tiFinal ([], _, _, _, _) = error "Empty stack is dectected"
tiFinal _ = False
#endif

isDataNode :: Node -> Bool
#if __CLH_EXERCISE_2__ < 21
#if __CLH_EXERCISE_2__ != 18
isDataNode (NNum n) = True
isDataNode node = False
#endif
#endif

step :: TiState -> TiState
#if __CLH_EXERCISE_2__ < 7
step state@(stack, dump, heap, globals, stats)
  = dispatch (hLookup heap (head stack))
  where
    dispatch (NNum n) = numStep state n
    dispatch (NAp a1 a2) = apStep state a1 a2
    dispatch (NSc scName argNames body) = scStep state scName argNames body
#endif

numStep :: TiState -> Int -> TiState
#if __CLH_EXERCISE_2__ < 16
numStep state n = error "Num applied as a function!"
#endif

apStep :: TiState -> Addr -> Addr -> TiState
#if __CLH_EXERCISE_2__ < 16
apStep (stack, dump, heap, globals, stats) a1 _
  = (a1 : stack, dump, heap, globals, stats)
#endif

scStep :: TiState -> Name -> [Name] -> CoreExpr -> TiState
#if __CLH_EXERCISE_2__ < 6
scStep (stack, dump, heap, globals, stats) scName argNames body
  = (stack', dump, heap', globals, stats)
  where
    stack' = resultAddr : drop (length argNames + 1) stack
    (heap', resultAddr) = instantiate body heap env
    env = argBindings ++ globals
    argBindings = zip argNames (getArgs heap stack)
#endif

getArgs :: TiHeap -> TiStack -> [Addr]
#if __CLH_EXERCISE_2__ < 7
getArgs heap (_ : stack)
  = map getArg stack
  where
    getArg a
      = case hLookup heap a of
          NAp _ arg -> arg
          _ -> error "Cannot get arg from non-application node"
getArgs _ _ = error "Cannot get args from empty stack"
#endif

instantiate :: CoreExpr -> TiHeap -> TiGlobals -> (TiHeap, Addr)
#if __CLH_EXERCISE_2__ < 7
instantiate (ENum n) heap env = hAlloc heap (NNum n)
instantiate (EAp e1 e2) heap env
  = hAlloc heap2 (NAp a1 a2)
  where
    (heap1, a1) = instantiate e1 heap env
    (heap2, a2) = instantiate e2 heap1 env
instantiate (EVar v) heap env
  = (heap, aLookup env v (error ("Undefined name " ++ v)))
instantiate (EConstr tag arity) heap env
  = instantiateConstr tag arity heap env
instantiate (ELet isRec defs body) heap env
  = instantiateLet isRec defs body heap env
instantiate (ECase e alts) heap env
  = error "Can't instantiate case exprs"
#endif

instantiateConstr :: Int -> Int -> TiHeap -> TiGlobals -> (TiHeap, Addr)
#if __CLH_EXERCISE_2__ < 21
#if __CLH_EXERCISE_2__ != 18
instantiateConstr = error "Can't instantiate constructors yet"
#endif
#endif

instantiateLet :: IsRec -> Assoc Name CoreExpr -> CoreExpr -> TiHeap -> TiGlobals -> (TiHeap, Addr)
#if __CLH_EXERCISE_2__ < 10
instantiateLet = error "Can't instantiate let(rec)s yet"
#endif

#if __CLH_EXERCISE_2__ < 26
showResults states
  = iDisplay resultSeq
  where
    resultSeq
      = iConcat [ iLayn (map showState states)
                , showStats (last states)
                ]
#endif

showState :: TiState -> ISeq
#if __CLH_EXERCISE_2__ < 5
showState (stack, _, heap, _, _)
  = iConcat [ showStack heap stack, iNewline ]
#endif

showStack :: TiHeap -> TiStack -> ISeq
#if __CLH_EXERCISE_2__ < 7
showStack heap stack
  = iConcat [ iStr "Stk ["
            , iIndent (iInterleave iNewline (map showStackItem stack))
            , iStr "]"
            ]
  where
    showStackItem addr
      = iConcat [ showFWAddr addr, iStr ": ", showStkNode heap (hLookup heap addr) ]
#endif

showStkNode :: TiHeap -> Node -> ISeq
#if __CLH_EXERCISE_2__ < 7
showStkNode heap (NAp funAddr argAddr)
  = iConcat [ iStr "NAp ", showFWAddr funAddr, iStr " ", showFWAddr argAddr
            , iStr " (", showNode (hLookup heap argAddr), iStr ")"
            ]
showStkNode heap node = showNode node
#endif

#if __CLH_EXERCISE_2__ < 13
showNode :: Node -> ISeq
showNode (NAp a1 a2)
  = iConcat [ iStr "NAp ", showAddrToSeq a1, iStr " ", showAddrToSeq a2 ]
showNode (NSc scName argNames body) = iStr ("NSc " ++ scName)
showNode (NNum n) = iStr "NNum " `iAppend` iNum n
#endif

-- |
-- Name is changed from `showAddr` to `showAddrToSeq` to avoid
-- name collision.
showAddrToSeq :: Addr -> ISeq
showAddrToSeq addr = iStr (showAddr addr)

showFWAddr :: Addr -> ISeq
showFWAddr addr = iStr (space (4 - length str) ++ str)
  where
    str = showAddr addr

showStats :: TiState -> ISeq
#if __CLH_EXERCISE_2__ < 7
showStats (_, _, _, _, stats)
  = iConcat [ iNewline
            , iNewline
            , iStr "Total number of steps = ", iNum (tiStatGetSteps stats)
            ]
#endif

#if __CLH_EXERCISE_2__ >= 5
#if __CLH_EXERCISE_2__ < 26
showState (stack, _, heap, _, _)
  = iConcat [ showStack heap stack, iNewline
#if __CLH_EXERCISE_2__ == 6
            , showHeap heap, iNewline
#endif
            ]
#endif

showHeap :: TiHeap -> ISeq
#if __CLH_EXERCISE_2__ < 7
showHeap heap
  = iConcat [ iStr "Heap ["
            , iIndent (iInterleave iNewline (map showHeapItem (hAddresses heap)))
            , iStr "]"
            ]
    where
      showHeapItem addr
        = iConcat [ showFWAddr addr, iStr ": ", showStkNode heap (hLookup heap addr) ]

#if __CLH_EXERCISE_2__ >= 6
scStep (stack, dump, heap, globals, stats) scName argNames body
  | argsLength + 1 <= length stack = (stack', dump, heap', globals, stats)
  | otherwise = error ("Two few arguments are provided to the function " ++ scName)
  where
    stack' = resultAddr : drop (argsLength + 1) stack
    (heap', resultAddr) = instantiate body heap env
    env = argBindings ++ globals
    argBindings = zip argNames (getArgs heap stack)
    argsLength = length argNames
#endif
#endif

#if __CLH_EXERCISE_2__ >= 7
type TiHeap = StatHeap Node

type TiStats
  = ( Int -- The number of steps
    , ( Int -- The number of supercombinator reduction
      , Int -- The number of primitive reduction
      )
    , Int -- The maximun stack depth
    )

tiStatInitial = (0, (0, 0), 0)
tiStatIncSteps (steps, redStats, maxStackDepth)
  = (steps + 1, redStats, maxStackDepth)
tiStatGetSteps (steps, _, _) = steps

tiStatIncScReds :: TiStats -> TiStats
tiStatIncScReds (steps, (scReds, pReds), maxStackDepth)
  = (steps, (scReds + 1, pReds), maxStackDepth)
tiStatGetScReds :: TiStats -> Int
tiStatGetScReds (_, (scReds, _), _)
  = scReds
tiStatIncPReds :: TiStats -> TiStats
tiStatIncPReds (steps, (scReds, pReds), maxStackDepth)
  = (steps, (scReds, pReds + 1), maxStackDepth)
tiStatGetPReds :: TiStats -> Int
tiStatGetPReds (_, (_, pReds), _)
  = pReds
tiStatSetMaxStackDepth :: Int -> TiStats -> TiStats
tiStatSetMaxStackDepth max (steps, (scReds, pReds), _)
  = (steps, (scReds, pReds), max)
tiStatGetMaxStackDepth :: TiStats -> Int
tiStatGetMaxStackDepth (_, _, maxStackDepth)
  = maxStackDepth

#if __CLH_EXERCISE_2__ < 16
buildInitialHeap = mapAccumL allocateSc statHInitial
#endif

allocateSc heap (name, args, body)
  = (heap', (name, addr))
  where
    (heap', addr) = statHAlloc heap (NSc name args body)

#if __CLH_EXERCISE_2__ < 26
doAdmin state@(stack, _, _, _, stats)
  = applyToStats (updateMaxStackDepth . tiStatIncSteps) state
  where
    updateMaxStackDepth
      | stackDepth <= statMaxStackDepth = id
      | otherwise = tiStatSetMaxStackDepth stackDepth

    stackDepth = length stack
    statMaxStackDepth = tiStatGetMaxStackDepth stats
#endif

#if __CLH_EXERCISE_2__ < 16
tiFinal ([soleAddr], _, heap, _, _) = isDataNode (statHLookup heap soleAddr)
tiFinal ([], _, _, _, _) = error "Empty stack is dectected"
tiFinal _ = False
#endif

#if __CLH_EXERCISE_2__ < 13
step state@(stack, dump, heap, globals, stats)
  = dispatch (statHLookup heap (head stack))
  where
    dispatch (NNum n) = numStep state n
    dispatch (NAp a1 a2) = apStep state a1 a2
    dispatch (NSc scName argNames body)
      = tiStatIncScReds `applyToStats` scStep state scName argNames body
#endif

getArgs heap (_ : stack)
  = map getArg stack
  where
    getArg a
      = case statHLookup heap a of
          NAp _ arg -> arg
          _ -> error "Cannot get arg from non-application node"
getArgs _ _ = error "Cannot get args from empty stack"

#if __CLH_EXERCISE_2__ != 18
instantiate (ENum n) heap env = statHAlloc heap (NNum n)
instantiate (EAp e1 e2) heap env
  = statHAlloc heap2 (NAp a1 a2)
  where
    (heap1, a1) = instantiate e1 heap env
    (heap2, a2) = instantiate e2 heap1 env
instantiate (EVar v) heap env
  = (heap, aLookup env v (error ("Undefined name " ++ v)))
instantiate (EConstr tag arity) heap env
  = instantiateConstr tag arity heap env
instantiate (ELet isRec defs body) heap env
  = instantiateLet isRec defs body heap env
instantiate (ECase e alts) heap env
  = error "Can't instantiate case exprs"
#endif

showStack heap stack
  = iConcat [ iStr "Stk ["
            , iIndent (iInterleave iNewline (map showStackItem stack))
            , iStr "]"
            ]
  where
    showStackItem addr
      = iConcat [ showFWAddr addr, iStr ": ", showStkNode heap (statHLookup heap addr) ]

#if __CLH_EXERCISE_2__ < 13
showStkNode heap (NAp funAddr argAddr)
  = iConcat [ iStr "NAp ", showFWAddr funAddr, iStr " ", showFWAddr argAddr
            , iStr " (", showNode (statHLookup heap argAddr), iStr ")"
            ]
showStkNode heap node = showNode node
#endif

#if __CLH_EXERCISE_2__ < 26
showStats (_, _, heap, _, stats)
  = iConcat [ iNewline
            , iNewline
            , iStr "Total number of steps                        : ", iNum steps, iNewline
            , iNewline
            , iStr "Total number of reductions                   : ", iNum (scReds + pReds), iNewline
            , iStr "Total number of supercombinator reductions   : ", iNum scReds, iNewline
            , iStr "Total number of primitive reductions         : ", iNum pReds, iNewline
            , showStatHeapStats heap, iNewline
            , iNewline
            , iStr "Maximum stack depth                          : ", iNum maxStackDepth
            ]
    where
      steps = tiStatGetSteps stats
      scReds = tiStatGetScReds stats
      pReds = tiStatGetPReds stats
      maxStackDepth = tiStatGetMaxStackDepth stats
#endif

showStatHeapStats :: TiHeap -> ISeq
showStatHeapStats heap
  = iConcat [ iNewline
            , iStr "Total number of heap allocations             : ", iNum allocations, iNewline
            , iStr "Total number of heap updates                 : ", iNum updates, iNewline
            , iStr "Total number of heap frees                   : ", iNum frees
            ]
    where
      allocations = statHSGetHAlloc stats
      updates = statHSGetHUpdate stats
      frees = statHSGetHFree stats
      stats = statHGetStats heap

showHeap heap
  = iConcat [ iStr "Heap ["
            , iIndent (iInterleave iNewline (map showHeapItem (statHAddresses heap)))
            , iStr "]"
            ]
    where
      showHeapItem addr
        = iConcat [ showFWAddr addr, iStr ": ", showStkNode heap (statHLookup heap addr) ]

#if __CLH_EXERCISE_2__ < 13
scStep (stack, dump, heap, globals, stats) scName argNames body
  | argsLength + 1 <= length stack = (stack', dump, heap', globals, stats)
  | otherwise = error ("Two few arguments are provided to the function " ++ scName)
  where
    stack' = resultAddr : drop (argsLength + 1) stack
    (heap', resultAddr) = instantiate body heap env
    env = argBindings ++ globals
    argBindings = zip argNames (getArgs heap stack)
    argsLength = length argNames
#endif

#if __CLH_EXERCISE_2__ >= 10
#if __CLH_EXERCISE_2__ < 11
instantiateLet isRec defs body heap env
  | not isRec = instantiate body heap' env'
  where
    (heap', defBindings) = mapAccumL (instantiateDef env) heap defs
    env' = defBindings ++ env
#endif

instantiateDef :: TiGlobals -> TiHeap -> (Name, CoreExpr) -> (TiHeap, (Name, Addr))
instantiateDef env heap (name, body)
  = (heap', (name, addr))
  where
    (heap', addr) = instantiate body heap env

#if __CLH_EXERCISE_2__ >= 11
instantiateLet isRec defs body heap env = instantiate body heap' env'
  where
    (heap', defBindings) = mapAccumL allocateDef heap defs
    allocateDef = instantiateDef (if isRec then env' else env)
    env' = defBindings ++ env

#if __CLH_EXERCISE_2__ >= 13
#if __CLH_EXERCISE_2__ < 16
data Node
  = NAp Addr Addr
  | NSc Name [Name] CoreExpr
  | NNum Int
  | NInd Addr
#endif

showNode :: TiHeap -> Node -> ISeq
#if __CLH_EXERCISE_2__ < 16
showNode _ (NAp a1 a2)
  = iConcat [ iStr "NAp ", showAddrToSeq a1, iStr " ", showAddrToSeq a2 ]
showNode _ (NSc scName argNames body) = iStr ("NSc " ++ scName)
showNode _ (NNum n) = iStr "NNum " `iAppend` iNum n
showNode heap (NInd a)
  = iConcat [ iStr "NInd (", showNode heap (statHLookup heap a), iStr ")" ]
#endif

showStkNode heap (NAp funAddr argAddr)
  = iConcat [ iStr "NAp ", showFWAddr funAddr, iStr " ", showFWAddr argAddr
            , iStr " (", showNode heap (statHLookup heap argAddr), iStr ")"
            ]
showStkNode heap node = showNode heap node

#if __CLH_EXERCISE_2__ < 14
scStep (stack, dump, heap, globals, stats) scName argNames body
  | argsLength + 1 <= length stack = (stack'', dump, heap'', globals, stats)
  | otherwise = error ("Two few arguments are provided to the function " ++ scName)
  where
    rootAddr : stack' = drop argsLength stack
    stack'' = resultAddr : stack'
    (heap', resultAddr) = instantiate body heap env
    heap'' = statHUpdate heap' rootAddr (NInd resultAddr)
    env = argBindings ++ globals
    argBindings = zip argNames (getArgs heap stack)
    argsLength = length argNames
#endif

#if __CLH_EXERCISE_2__ < 16
step state@(stack, dump, heap, globals, stats)
  = dispatch (statHLookup heap (head stack))
  where
    dispatch (NNum n) = numStep state n
    dispatch (NAp a1 a2) = apStep state a1 a2
    dispatch (NSc scName argNames body)
      = tiStatIncScReds `applyToStats` scStep state scName argNames body
    dispatch (NInd addr) = indStep state addr
#endif

indStep :: TiState -> Addr -> TiState
#if __CLH_EXERCISE_2__ < 26
indStep (_ : stack, dump, heap, globals, stats) addr
  = (addr : stack, dump, heap, globals, stats)
indStep _ _ = error "Empty stack for indirection is detected"
#endif

instantiateAndUpdate :: CoreExpr -> Addr -> TiHeap -> TiGlobals -> TiHeap
#if __CLH_EXERCISE_2__ != 18
instantiateAndUpdate (EAp e1 e2) updateAddr heap env
  = statHUpdate heap2 updateAddr (NAp a1 a2)
  where
    (heap1, a1) = instantiate e1 heap env
    (heap2, a2) = instantiate e2 heap1 env
#if __CLH_EXERCISE_2__ >= 14
instantiateAndUpdate (ENum n) updateAddr heap env = statHUpdate heap updateAddr (NNum n)
instantiateAndUpdate (EVar v) updateAddr heap env
  = statHUpdate heap updateAddr (NInd vAddr)
  where
    vAddr = aLookup env v (error ("Undefined name " ++ v))
instantiateAndUpdate (EConstr tag arity) updateAddr heap env
  = instantiateAndUpdateConstr tag arity updateAddr heap env
instantiateAndUpdate (ELet isRec defs body) updateAddr heap env
  = instantiateAndUpdateLet isRec defs body updateAddr heap env
instantiateAndUpdate (ECase e alts) updateAddr heap env
  = error "Can't instantiate case exprs"
#endif
#endif

#if __CLH_EXERCISE_2__ >= 14
instantiateAndUpdateConstr :: Int -> Int -> Addr -> TiHeap -> TiGlobals -> TiHeap
#if __CLH_EXERCISE_2__ < 21
#if __CLH_EXERCISE_2__ != 18
instantiateAndUpdateConstr = error "Can't instantiate constructors yet"
#endif
#endif

instantiateAndUpdateLet :: IsRec -> Assoc Name CoreExpr -> CoreExpr -> Addr -> TiHeap -> TiGlobals -> TiHeap
instantiateAndUpdateLet isRec defs body addr heap env = instantiateAndUpdate body addr heap' env'
  where
    (heap', defBindings) = mapAccumL allocateDef heap defs
    allocateDef = instantiateDef (if isRec then env' else env)
    env' = defBindings ++ env

#if __CLH_EXERCISE_2__ < 26
scStep (stack, dump, heap, globals, stats) scName argNames body
  | argsLength + 1 <= length stack = (stack', dump, heap', globals, stats)
  | otherwise = error ("Two few arguments are provided to the function " ++ scName)
  where
    stack'@(rootAddr : _) = drop argsLength stack
    heap' = instantiateAndUpdate body rootAddr heap env
    env = argBindings ++ globals
    argBindings = zip argNames (getArgs heap stack)
    argsLength = length argNames
#endif

#if __CLH_EXERCISE_2__ >= 16
type TiDump = [TiStack]
initialTiDump = []

#if __CLH_EXERCISE_2__ < 21
#if __CLH_EXERCISE_2__ != 18
data Node
  = NAp Addr Addr
  | NSc Name [Name] CoreExpr
  | NNum Int
  | NInd Addr
  | NPrim Name Primitive

data Primitive = Neg | Add | Sub | Mul | Div
#endif
#endif

buildInitialHeap scDefs
  = (heap2, scAddrs ++ primAddrs)
  where
    (heap1, scAddrs) = mapAccumL allocateSc statHInitial scDefs
    (heap2, primAddrs) = mapAccumL allocatePrim heap1 primitives

primitives :: Assoc Name Primitive
#if __CLH_EXERCISE_2__ < 21
primitives
  = [ ("negate", Neg)
    , ("+", Add), ("-", Sub)
    , ("*", Mul), ("/", Div)
    ]
#endif

allocatePrim :: TiHeap -> (Name, Primitive) -> (TiHeap, (Name, Addr))
allocatePrim heap (name, prim) = (heap', (name, addr))
  where
    (heap', addr) = statHAlloc heap (NPrim name prim)

#if __CLH_EXERCISE_2__ < 21
#if __CLH_EXERCISE_2__ != 18
step state@(stack, dump, heap, globals, stats)
  = dispatch (statHLookup heap (head stack))
  where
    dispatch (NNum n) = numStep state n
    dispatch (NAp a1 a2) = apStep state a1 a2
    dispatch (NSc scName argNames body)
      = tiStatIncScReds `applyToStats` scStep state scName argNames body
    dispatch (NInd addr) = indStep state addr
    dispatch (NPrim _ prim)
      = tiStatIncPReds `applyToStats` primStep state prim
#endif
#endif

primStep :: TiState -> Primitive -> TiState
#if __CLH_EXERCISE_2__ < 17
primStep state Neg = primNeg state
#endif

-- Do we need to check stack length?
-- It should be longer than or equal to 2
primNeg :: TiState -> TiState
#if __CLH_EXERCISE_2__ < 26
#if __CLH_EXERCISE_2__ < 21
#if __CLH_EXERCISE_2__ != 18
primNeg (stack@(_ : _ : _), dump, heap, globals, stats)
  | isDataNode arg = (negApStack, dump, heap', globals, stats)
  | otherwise = ([argAddr], negApStack : dump, heap, globals, stats)
  where
    _ : negApStack@(rootAddr : _) = stack

    heap' = statHUpdate heap rootAddr (NNum (negate value))

    argAddr : _ = getArgs heap stack
    arg = statHLookup heap argAddr
    NNum value = arg
primNeg _ = error "Wrong stack for negate is detected"
#endif
#endif

numStep ([_], stack : dump, heap, globals, stats) _
  = (stack, dump, heap, globals, stats)
numStep (stack, _ : _, heap, _, _) _
  = error ("Wrong stack is detected : " ++ iDisplay (showStack heap stack))
numStep (_, dump, heap, _, _) _
  = error ("Wrong dump is detected : " ++ iDisplay (iInterleave iNewline (map (showStack heap) dump)))

apStep (stack@(topAddr : _), dump, heap, globals, stats) a1 a2
  = case arg of
      NInd a3 -> (stack, dump, makeHeap a3, globals, stats)
      _ -> (a1 : stack, dump, heap, globals, stats)
  where
    makeHeap = statHUpdate heap topAddr . NAp a1
    arg = statHLookup heap a2
apStep _ _ _ = error "Empty stack for application is dectected"

tiFinal ([soleAddr], [], heap, _, _) = isDataNode (statHLookup heap soleAddr)
tiFinal ([], _, _, _, _) = error "Empty stack is dectected"
tiFinal _ = False
#endif

#if __CLH_EXERCISE_2__ < 21
#if __CLH_EXERCISE_2__ != 18
showNode _ (NAp a1 a2)
  = iConcat [ iStr "NAp ", showAddrToSeq a1, iStr " ", showAddrToSeq a2 ]
showNode _ (NSc scName argNames body) = iStr ("NSc " ++ scName)
showNode _ (NNum n) = iStr "NNum " `iAppend` iNum n
showNode heap (NInd a)
  = iConcat [ iStr "NInd (", showNode heap (statHLookup heap a), iStr ")" ]
showNode heap (NPrim name _)
  = iConcat [ iStr "NPrim ", iStr name ]
#endif
#endif

#if __CLH_EXERCISE_2__ >= 17
#if __CLH_EXERCISE_2__ < 21
#if __CLH_EXERCISE_2__ != 18
primStep state Neg = primNeg state
primStep state Add = primArith state (+)
primStep state Sub = primArith state (-)
primStep state Mul = primArith state (*)
primStep state Div = primArith state div
#endif
#endif

primArith :: TiState -> (Int -> Int -> Int) -> TiState
#if __CLH_EXERCISE_2__ < 21
#if __CLH_EXERCISE_2__ != 18
primArith (stack@(_ : _ : _ : _), dump, heap, globals, stats) f
  | arg1IsDataNode && arg2IsDataNode = (ap2Stack, dump, heap', globals, stats)
  | arg2IsDataNode = ([arg1Addr], ap1Stack : dump, heap, globals, stats)
  | otherwise = ([arg2Addr], ap2Stack : dump, heap, globals, stats)
  where
    heap' = statHUpdate heap rootAddr (NNum (f value1 value2))

    _ : ap1Stack = stack
    _ : ap2Stack = ap1Stack
    rootAddr : _ = ap2Stack

    arg1Addr : arg2Addr : _ = getArgs heap stack
    arg1 = statHLookup heap arg1Addr
    arg2 = statHLookup heap arg2Addr
    arg1IsDataNode = isDataNode arg1
    arg2IsDataNode = isDataNode arg2
    NNum value1 = arg1
    NNum value2 = arg2
primArith _ _ = error "Wrong stack for an arithmetic binary operator is detected"
#endif
#endif

#if __CLH_EXERCISE_2__ == 18
data Node
  = NAp Addr Addr
  | NSc Name [Name] CoreExpr
  | NNum Int
  | NInd Addr
  | NPrim Name Primitive
  | NData Int [Addr]
  | NCase Addr [Addr]
  | NAlter Int [Name] CoreExpr

showNode _ (NAp a1 a2)
  = iConcat [ iStr "NAp ", showAddrToSeq a1, iStr " ", showAddrToSeq a2 ]
showNode _ (NSc scName argNames body) = iStr ("NSc " ++ scName)
showNode _ (NNum n) = iStr "NNum " `iAppend` iNum n
showNode heap (NInd a)
  = iConcat [ iStr "NInd (", showNode heap (statHLookup heap a), iStr ")" ]
showNode heap (NPrim name _)
  = iConcat [ iStr "NPrim ", iStr name ]
showNode heap (NData tag args)
  = iConcat [ iStr "NData ", iNum tag, iStr ", ", iInterleave (iStr " ") (map showFWAddr args) ]
showNode heap (NCase eAddr alterAddrs)
  = iConcat [ iStr "NCase ", showFWAddr eAddr, iStr " of ", iInterleave (iStr " ") (map showFWAddr alterAddrs) ]
showNode heap (NAlter tag argNames body)
  = iConcat [ iStr "NAlter ", iNum tag, iStr ", ", iInterleave (iStr " ") (map iStr argNames) ]

instantiate (ENum n) heap env = statHAlloc heap (NNum n)
instantiate (EAp e1 e2) heap env
  = statHAlloc heap2 (NAp a1 a2)
  where
    (heap1, a1) = instantiate e1 heap env
    (heap2, a2) = instantiate e2 heap1 env
instantiate (EVar v) heap env
  = (heap, aLookup env v (error ("Undefined name " ++ v)))
instantiate (EConstr tag arity) heap env
  = instantiateConstr tag arity heap env
instantiate (ELet isRec defs body) heap env
  = instantiateLet isRec defs body heap env
instantiate (ECase e alts) heap env
  = instantiateCase e alts heap env

instantiateCase :: CoreExpr -> [CoreAlter] -> TiHeap -> TiGlobals -> (TiHeap, Addr)
instantiateCase e alts heap env
  = statHAlloc heap'' (NCase eAddr altAddrs)
  where
    (heap', eAddr) = instantiate e heap env
    (heap'', altAddrs) = mapAccumL allocateAlter heap' alts

allocateAlter :: TiHeap -> CoreAlter -> (TiHeap, Addr)
allocateAlter heap (tag, argNames, body) = statHAlloc heap (NAlter tag argNames body)

instantiateAndUpdate (EAp e1 e2) updateAddr heap env
  = statHUpdate heap2 updateAddr (NAp a1 a2)
  where
    (heap1, a1) = instantiate e1 heap env
    (heap2, a2) = instantiate e2 heap1 env
instantiateAndUpdate (ENum n) updateAddr heap env = statHUpdate heap updateAddr (NNum n)
instantiateAndUpdate (EVar v) updateAddr heap env
  = statHUpdate heap updateAddr (NInd vAddr)
  where
    vAddr = aLookup env v (error ("Undefined name " ++ v))
instantiateAndUpdate (EConstr tag arity) updateAddr heap env
  = instantiateAndUpdateConstr tag arity updateAddr heap env
instantiateAndUpdate (ELet isRec defs body) updateAddr heap env
  = instantiateAndUpdateLet isRec defs body updateAddr heap env
instantiateAndUpdate (ECase e alts) updateAddr heap env
  = instantiateAndUpdateCase e alts updateAddr heap env

instantiateAndUpdateCase :: CoreExpr -> [CoreAlter] -> Addr -> TiHeap -> TiGlobals -> TiHeap
instantiateAndUpdateCase e alts updateAddr heap env
  = statHUpdate heap'' updateAddr (NCase eAddr altAddrs)
  where
    (heap', eAddr) = instantiate e heap env
    (heap'', altAddrs) = mapAccumL allocateAlter heap' alts

data Primitive
  = Neg
  | Add
  | Sub
  | Mul
  | Div
  | PrimConstr Int Int

instantiateConstr tag arity heap env = (heap', addr)
  where
    (heap', addr) = statHAlloc heap (NPrim "Pack" (PrimConstr tag arity))
instantiateAndUpdateConstr tag arity addr heap env = heap'
  where
    heap' = statHUpdate heap addr (NPrim "Pack" (PrimConstr tag arity))

isDataNode (NNum n) = True
isDataNode (NData tag args) = True
isDataNode node = False

step state@(stack, dump, heap, globals, stats)
  = dispatch (statHLookup heap (head stack))
  where
    dispatch (NNum n) = numStep state n
    dispatch (NAp a1 a2) = apStep state a1 a2
    dispatch (NSc scName argNames body)
      = tiStatIncScReds `applyToStats` scStep state scName argNames body
    dispatch (NInd addr) = indStep state addr
    dispatch (NPrim _ prim)
      = tiStatIncPReds `applyToStats` primStep state prim
    dispatch (NData tag args) = dataStep state tag args
    dispatch (NCase eAddr alterAddrs) = caseStep state eAddr alterAddrs

dataStep :: TiState -> Int -> [Addr] -> TiState
dataStep ([_], stack : dump, heap, globals, stats) _ _
  = (stack, dump, heap, globals, stats)
dataStep (stack, _ : dump, heap, globals, stats) _ _
  = error ("Wrong stack is detected : " ++ iDisplay (showStack heap stack))
dataStep (_, dump, heap, globals, stats) _ _
  = error ("Wrong dump is detected : " ++ iDisplay (iInterleave iNewline (map (showStack heap) dump)))

caseStep :: TiState -> Addr -> [Addr] -> TiState
caseStep (stack, dump, heap, globals, stats) eAddr alterAddrs
  | isDataNode expr = (stack, dump, heap', globals, stats)
  | otherwise = ([eAddr], stack : dump, heap, globals, stats)
  where
    rootAddr : stack' = stack
    heap' = instantiateAndUpdate body rootAddr heap env
    env = argBindings ++ globals

    alters = map (statHLookup heap) alterAddrs
    NAlter _ argNames body : _ = filter findAlter alters

    expr = statHLookup heap eAddr
    NData tag argAddrs = expr

    argsLength = length argAddrs
    argBindings = zip argNames argAddrs

    findAlter (NAlter alterTag argNames _) = alterTag == tag && length argNames == argsLength

primStep state Neg = primNeg state
primStep state Add = primArith state (+)
primStep state Sub = primArith state (-)
primStep state Mul = primArith state (*)
primStep state Div = primArith state div
primStep state (PrimConstr tag arity) = primConstr state tag arity

primNeg (stack@(_ : _ : _), dump, heap, globals, stats)
  = case arg of
      NNum v -> (negApStack, dump, makeHeap v, globals, stats)
      _
        | isDataNode arg -> error "Negation cannot be applied to other than numbers"
        | otherwise -> ([argAddr], negApStack : dump, heap, globals, stats)
  where
    _ : negApStack@(rootAddr : _) = stack

    makeHeap = statHUpdate heap rootAddr . NNum . negate

    argAddr : _ = getArgs heap stack
    arg = statHLookup heap argAddr
primNeg _ = error "Wrong stack for negate is detected"

primArith (stack@(_ : _ : _ : _), dump, heap, globals, stats) f
  = case (arg1, arg2) of
      (NNum v1, NNum v2) -> (ap2Stack, dump, makeHeap v1 v2, globals, stats)
      (_, NNum v2)
        | isDataNode arg1 -> error "An arithmetic binary operator cannot be applied to other than numbers"
        | otherwise -> ([arg1Addr], ap1Stack : dump, heap, globals, stats)
      _
        | isDataNode arg2 -> error "An arithmetic binary operator cannot be applied to other than numbers"
        | otherwise -> ([arg2Addr], ap2Stack : dump, heap, globals, stats)
  where
    makeHeap v1 = statHUpdate heap rootAddr . NNum . f v1

    _ : ap1Stack = stack
    _ : ap2Stack = ap1Stack
    rootAddr : _ = ap2Stack

    arg1Addr : arg2Addr : _ = getArgs heap stack
    arg1 = statHLookup heap arg1Addr
    arg2 = statHLookup heap arg2Addr
primArith _ _ = error "Wrong stack for an arithmetic binary operator Is detected"

primConstr :: TiState -> Int -> Int -> TiState
primConstr (stack, dump, heap, globals, stats) tag arity
  | length stack >= arity + 1 = (stack', dump, heap', globals, stats)
  | otherwise = error "Wrong stack for data type construction is detected"
  where
    stack'@(rootAddr : _) = drop arity stack
    heap' = statHUpdate heap rootAddr (NData tag args)
    args = take arity $ getArgs heap stack
#endif

#if __CLH_EXERCISE_2__ >= 20
#if __CLH_EXERCISE_2__ < 22
extraPreludeDefs
  = [ ("False", [], EConstr 1 0)
    , ("True", [], EConstr 2 0)
    , ("and", ["x", "y"], EAp (EAp (EAp (EVar "if") (EVar "x")) (EVar "y")) (EVar "False"))
    , ("or", ["x", "y"], EAp (EAp (EAp (EVar "if") (EVar "x")) (EVar "True")) (EVar "y"))
    , ("xor", ["x", "y"], EAp (EAp (EAp (EVar "if") (EVar "x")) (EAp (EVar "not") (EVar "y"))) (EVar "y"))
    , ("not", ["y"], EAp (EAp (EAp (EVar "if") (EVar "y")) (EVar "False")) (EVar "True"))
    ]
#endif

#if __CLH_EXERCISE_2__ >= 21
data Node
  = NAp Addr Addr
  | NSc Name [Name] CoreExpr
  | NNum Int
  | NInd Addr
  | NPrim Name Primitive
  | NData Int [Addr]

showNode _ (NAp a1 a2)
  = iConcat [ iStr "NAp ", showAddrToSeq a1, iStr " ", showAddrToSeq a2 ]
showNode _ (NSc scName argNames body) = iStr ("NSc " ++ scName)
showNode _ (NNum n) = iStr "NNum " `iAppend` iNum n
showNode heap (NInd a)
  = iConcat [ iStr "NInd (", showNode heap (statHLookup heap a), iStr ")" ]
showNode heap (NPrim name _)
  = iConcat [ iStr "NPrim ", iStr name ]
showNode heap (NData tag args)
  = iConcat [ iStr "NData ", iNum tag, iStr ", ", iInterleave (iStr " ") (map showFWAddr args) ]

#if __CLH_EXERCISE_2__ < 26
primNeg (stack@(_ : _ : _), dump, heap, globals, stats)
  = case arg of
      NNum v -> (negApStack, dump, makeHeap v, globals, stats)
      _
        | isDataNode arg -> error "Negation cannot be applied to other than numbers"
        | otherwise -> ([argAddr], negApStack : dump, heap, globals, stats)
  where
    _ : negApStack@(rootAddr : _) = stack

    makeHeap = statHUpdate heap rootAddr . NNum . negate

    argAddr : _ = getArgs heap stack
    arg = statHLookup heap argAddr
primNeg _ = error "Wrong stack for negate is detected"
#endif

#if __CLH_EXERCISE_2__ < 22
data Primitive
  = Neg
  | Add
  | Sub
  | Mul
  | Div
  | PrimConstr Int Int
  | If
  | Greater
  | GreaterEq
  | Less
  | LessEq
  | Eq
  | NotEq

primitives
  = [ ("negate", Neg)
    , ("+", Add), ("-", Sub)
    , ("*", Mul), ("/", Div)
    , ("if", If)
    , (">", Greater), (">=", GreaterEq)
    , ("<", Less), ("<=", LessEq)
    , ("==", Eq), ("~=", NotEq)
    ]
#endif

instantiateConstr tag arity heap env = (heap', addr)
  where
    (heap', addr) = statHAlloc heap (NPrim "Pack" (PrimConstr tag arity))
instantiateAndUpdateConstr tag arity addr heap env = heap'
  where
    heap' = statHUpdate heap addr (NPrim "Pack" (PrimConstr tag arity))

isDataNode (NNum n) = True
isDataNode (NData tag args) = True
isDataNode node = False

#if __CLH_EXERCISE_2__ < 26
step state@(stack, dump, heap, globals, stats)
  = dispatch (statHLookup heap (head stack))
  where
    dispatch (NNum n) = numStep state n
    dispatch (NAp a1 a2) = apStep state a1 a2
    dispatch (NSc scName argNames body)
      = tiStatIncScReds `applyToStats` scStep state scName argNames body
    dispatch (NInd addr) = indStep state addr
    dispatch (NPrim _ prim)
      = tiStatIncPReds `applyToStats` primStep state prim
    dispatch (NData tag args) = dataStep state tag args
#endif

dataStep :: TiState -> Int -> [Addr] -> TiState
#if __CLH_EXERCISE_2__ < 26
dataStep ([_], stack : dump, heap, globals, stats) _ _
  = (stack, dump, heap, globals, stats)
dataStep (stack, _ : dump, heap, globals, stats) _ _
  = error ("Wrong stack is detected : " ++ iDisplay (showStack heap stack))
dataStep (_, dump, heap, globals, stats) _ _
  = error ("Wrong dump is detected : " ++ iDisplay (iInterleave iNewline (map (showStack heap) dump)))
#endif

#if __CLH_EXERCISE_2__ < 22
primStep state Neg = primNeg state
primStep state Add = primArith state (+)
primStep state Sub = primArith state (-)
primStep state Mul = primArith state (*)
primStep state Div = primArith state div
primStep state (PrimConstr tag arity) = primConstr state tag arity
primStep state If = primIf state
primStep state Greater = primComp state (>)
primStep state GreaterEq = primComp state (>=)
primStep state Less = primComp state (<)
primStep state LessEq = primComp state (<=)
primStep state Eq = primComp state (==)
primStep state NotEq = primComp state (/=)
#endif

primConstr :: TiState -> Int -> Int -> TiState
#if __CLH_EXERCISE_2__ < 26
primConstr (stack, dump, heap, globals, stats) tag arity
  | length stack >= arity + 1 = (stack', dump, heap', globals, stats)
  | otherwise = error "Wrong stack for data type construction is detected"
  where
    stack'@(rootAddr : _) = drop arity stack
    heap' = statHUpdate heap rootAddr (NData tag args)
    args = take arity $ getArgs heap stack
#endif

primIf :: TiState -> TiState
#if __CLH_EXERCISE_2__ < 26
primIf (stack@(_ : _ : _ : _ : _), dump, heap, globals, stats)
  = case cond of
      NData 1 [] -> (rootStack, dump, falseHeap, globals, stats)
      NData 2 [] -> (rootStack, dump, trueHeap, globals, stats)
      _
        | isDataNode cond -> error "Wrong data type for if is detected"
        | otherwise -> ([condAddr], ifApStack : dump, heap, globals, stats)
  where
    trueHeap = statHUpdate heap rootAddr (NInd trueAddr)
    falseHeap = statHUpdate heap rootAddr (NInd falseAddr)

    _ : ifApStack = stack
    _ : _ : rootStack = ifApStack
    rootAddr : _ = rootStack

    condAddr : trueAddr : falseAddr : _ = getArgs heap stack
    cond = statHLookup heap condAddr
primIf _ = error "Wrong stack for if is detected"
#endif

primArith state f = primDyadic state nodeF
  where
    nodeF (NNum v1) (NNum v2) = NNum (f v1 v2)
    nodeF _ _ = error "Wrong data type for a binary arithmetic operation is detected"

primComp :: TiState -> (Int -> Int -> Bool) -> TiState
primComp state f = primDyadic state nodeF
  where
    nodeF (NNum v1) (NNum v2)
      | f v1 v2 = NData 2 []
      | otherwise = NData 1 []
    nodeF _ _ = error "Wrong data type for a binary comparison operation is detected"

primDyadic :: TiState -> (Node -> Node -> Node) -> TiState
#if __CLH_EXERCISE_2__ < 26
primDyadic (stack@(_ : _ : _ : _), dump, heap, globals, stats) f
  | arg1IsDataNode && arg2IsDataNode = (ap2Stack, dump, heap', globals, stats)
  | arg2IsDataNode = ([arg1Addr], ap1Stack : dump, heap, globals, stats)
  | otherwise = ([arg2Addr], ap2Stack : dump, heap, globals, stats)
  where
    heap' = statHUpdate heap rootAddr (f arg1 arg2)

    _ : ap1Stack = stack
    _ : ap2Stack = ap1Stack
    rootAddr : _ = ap2Stack

    arg1Addr : arg2Addr : _ = getArgs heap stack
    arg1 = statHLookup heap arg1Addr
    arg2 = statHLookup heap arg2Addr
    arg1IsDataNode = isDataNode arg1
    arg2IsDataNode = isDataNode arg2
primDyadic _ _ = error "Wrong stack for a binary operation is detected"
#endif

#if __CLH_EXERCISE_2__ >= 22
#if __CLH_EXERCISE_2__ < 24
data Primitive
  = Neg
  | Add
  | Sub
  | Mul
  | Div
  | PrimConstr Int Int
  | If
  | Greater
  | GreaterEq
  | Less
  | LessEq
  | Eq
  | NotEq
  | CasePair

primitives
  = [ ("negate", Neg)
    , ("+", Add), ("-", Sub)
    , ("*", Mul), ("/", Div)
    , ("if", If)
    , (">", Greater), (">=", GreaterEq)
    , ("<", Less), ("<=", LessEq)
    , ("==", Eq), ("~=", NotEq)
    , ("casePair", CasePair)
    ]

primStep state Neg = primNeg state
primStep state Add = primArith state (+)
primStep state Sub = primArith state (-)
primStep state Mul = primArith state (*)
primStep state Div = primArith state div
primStep state (PrimConstr tag arity) = primConstr state tag arity
primStep state If = primIf state
primStep state Greater = primComp state (>)
primStep state GreaterEq = primComp state (>=)
primStep state Less = primComp state (<)
primStep state LessEq = primComp state (<=)
primStep state Eq = primComp state (==)
primStep state NotEq = primComp state (/=)
primStep state CasePair = primCasePair state
#endif

primCasePair :: TiState -> TiState
#if __CLH_EXERCISE_2__ < 26
primCasePair (stack@(_ : _ : _ : _), dump, heap, globals, stats)
  = case expr of
      NData 1 [arg1, arg2] -> (rootStack, dump, makeHeap arg1 arg2, globals, stats)
      _
        | isDataNode expr -> error "Wrong data type for casePair is detected"
        | otherwise -> ([exprAddr], caseApStack : dump, heap, globals, stats)
  where
    makeHeap arg1 arg2 = statHUpdate heap' rootAddr (NAp funAddr' arg2)
      where
        (heap', funAddr') = statHAlloc heap (NAp funAddr arg1)

    _ : caseApStack = stack
    _ : rootStack = caseApStack
    rootAddr : _ = rootStack

    exprAddr : funAddr : _ = getArgs heap stack
    expr = statHLookup heap exprAddr
primCasePair _ = error "Wrong stack for casePair is detected"
#endif

#if __CLH_EXERCISE_2__ < 24
extraPreludeDefs
  = [ ("False", [], EConstr 1 0)
    , ("True", [], EConstr 2 0)
    , ("and", ["x", "y"], EAp (EAp (EAp (EVar "if") (EVar "x")) (EVar "y")) (EVar "False"))
    , ("or", ["x", "y"], EAp (EAp (EAp (EVar "if") (EVar "x")) (EVar "True")) (EVar "y"))
    , ("xor", ["x", "y"], EAp (EAp (EAp (EVar "if") (EVar "x")) (EAp (EVar "not") (EVar "y"))) (EVar "y"))
    , ("not", ["y"], EAp (EAp (EAp (EVar "if") (EVar "y")) (EVar "False")) (EVar "True"))
    , ("MkPair", [], EConstr 1 2)
    , ("fst", ["p"], EAp (EAp (EVar "casePair") (EVar "p")) (EVar "K"))
    , ("snd", ["p"], EAp (EAp (EVar "casePair") (EVar "p")) (EVar "K1"))
    ]
#endif

#if __CLH_EXERCISE_2__ >= 24
#if __CLH_EXERCISE_2__ < 26
data Primitive
  = Neg
  | Add
  | Sub
  | Mul
  | Div
  | PrimConstr Int Int
  | If
  | Greater
  | GreaterEq
  | Less
  | LessEq
  | Eq
  | NotEq
  | CasePair
  | CaseList
  | Abort

primitives
  = [ ("negate", Neg)
    , ("+", Add), ("-", Sub)
    , ("*", Mul), ("/", Div)
    , ("if", If)
    , (">", Greater), (">=", GreaterEq)
    , ("<", Less), ("<=", LessEq)
    , ("==", Eq), ("~=", NotEq)
    , ("casePair", CasePair)
    , ("caseList", CaseList)
    , ("abort", Abort)
    ]

primStep state Neg = primNeg state
primStep state Add = primArith state (+)
primStep state Sub = primArith state (-)
primStep state Mul = primArith state (*)
primStep state Div = primArith state div
primStep state (PrimConstr tag arity) = primConstr state tag arity
primStep state If = primIf state
primStep state Greater = primComp state (>)
primStep state GreaterEq = primComp state (>=)
primStep state Less = primComp state (<)
primStep state LessEq = primComp state (<=)
primStep state Eq = primComp state (==)
primStep state NotEq = primComp state (/=)
primStep state CasePair = primCasePair state
primStep state CaseList = primCaseList state
primStep state Abort = error "Program is aborted by abort primitive"
#endif

primCaseList :: TiState -> TiState
#if __CLH_EXERCISE_2__ < 26
primCaseList (stack@(_ : _ : _ : _ : _), dump, heap, globals, stats)
  = case expr of
      NData 1 [] -> (rootStack, dump, nilHeap, globals, stats)
      NData 2 [h, t] -> (rootStack, dump, makeConsHeap h t, globals, stats)
      _
        | isDataNode expr -> error "Wrong data type for caseList is detected"
        | otherwise -> ([exprAddr], caseApStack : dump, heap, globals, stats)
  where
    nilHeap = statHUpdate heap rootAddr (NInd nilAddr)
    makeConsHeap h t = statHUpdate heap' rootAddr (NAp consAddr' t)
      where
        (heap', consAddr') = statHAlloc heap (NAp consAddr h)

    _ : caseApStack = stack
    _ : _ : rootStack = caseApStack
    rootAddr : _ = rootStack

    exprAddr : nilAddr : consAddr : _ = getArgs heap stack
    expr = statHLookup heap exprAddr
primCaseList _ = error "Wrong stack for caseList is detected"
#endif

#if __CLH_EXERCISE_2__ < 26
extraPreludeDefs
  = [ ("False", [], EConstr 1 0)
    , ("True", [], EConstr 2 0)
    , ("and", ["x", "y"], EAp (EAp (EAp (EVar "if") (EVar "x")) (EVar "y")) (EVar "False"))
    , ("or", ["x", "y"], EAp (EAp (EAp (EVar "if") (EVar "x")) (EVar "True")) (EVar "y"))
    , ("xor", ["x", "y"], EAp (EAp (EAp (EVar "if") (EVar "x")) (EAp (EVar "not") (EVar "y"))) (EVar "y"))
    , ("not", ["y"], EAp (EAp (EAp (EVar "if") (EVar "y")) (EVar "False")) (EVar "True"))
    , ("MkPair", [], EConstr 1 2)
    , ("fst", ["p"], EAp (EAp (EVar "casePair") (EVar "p")) (EVar "K"))
    , ("snd", ["p"], EAp (EAp (EVar "casePair") (EVar "p")) (EVar "K1"))
    , ("Cons", [], EConstr 2 2)
    , ("Nil", [], EConstr 1 0)
    , ("head", ["l"], EAp (EAp (EAp (EVar "caseList") (EVar "l")) (EVar "abort")) (EVar "K"))
    , ("tail", ["l"], EAp (EAp (EAp (EVar "caseList") (EVar "l")) (EVar "abort")) (EVar "K1"))
    ]
#endif

#if __CLH_EXERCISE_2__ >= 26
type TiState = (TiOutput, TiStack, TiDump, TiHeap, TiGlobals, TiStats)

type TiOutput = [Int]

applyToStats statFun (output, stack, dump, heap, scDefs, stats)
  = (output, stack, dump, heap, scDefs, statFun stats)

showState (_, stack, _, heap, _, _)
  = iConcat [ showStack heap stack, iNewline
#if __CLH_EXERCISE_2__ == 6
            , showHeap heap, iNewline
#endif
            ]

doAdmin state@(_, stack, _, _, _, stats)
  = applyToStats (updateMaxStackDepth . tiStatIncSteps) state
  where
    updateMaxStackDepth
      | stackDepth <= statMaxStackDepth = id
      | otherwise = tiStatSetMaxStackDepth stackDepth

    stackDepth = length stack
    statMaxStackDepth = tiStatGetMaxStackDepth stats

showStats (_, _, _, heap, _, stats)
  = iConcat [ iNewline
            , iNewline
            , iStr "Total number of steps                        : ", iNum steps, iNewline
            , iNewline
            , iStr "Total number of reductions                   : ", iNum (scReds + pReds), iNewline
            , iStr "Total number of supercombinator reductions   : ", iNum scReds, iNewline
            , iStr "Total number of primitive reductions         : ", iNum pReds, iNewline
            , showStatHeapStats heap, iNewline
            , iNewline
            , iStr "Maximum stack depth                          : ", iNum maxStackDepth
            ]
    where
      steps = tiStatGetSteps stats
      scReds = tiStatGetScReds stats
      pReds = tiStatGetPReds stats
      maxStackDepth = tiStatGetMaxStackDepth stats

indStep (output, _ : stack, dump, heap, globals, stats) addr
  = (output, addr : stack, dump, heap, globals, stats)
indStep _ _ = error "Wrong stack!"

scStep (output, stack, dump, heap, globals, stats) scName argNames body
  | argsLength + 1 <= length stack = (output, stack', dump, heap', globals, stats)
  | otherwise = error ("Two few arguments are provided to the function " ++ scName)
  where
    stack'@(rootAddr : _) = drop argsLength stack
    heap' = instantiateAndUpdate body rootAddr heap env
    env = argBindings ++ globals
    argBindings = zip argNames (getArgs heap stack)
    argsLength = length argNames

primNeg (output, stack@(_ : _ : _), dump, heap, globals, stats)
  = case arg of
      NNum v -> (output, negApStack, dump, makeHeap v, globals, stats)
      _
        | isDataNode arg -> error "Negation cannot be applied to other than numbers"
        | otherwise -> (output, [argAddr], negApStack : dump, heap, globals, stats)
  where
    _ : negApStack@(rootAddr : _) = stack

    makeHeap = statHUpdate heap rootAddr . NNum . negate

    argAddr : _ = getArgs heap stack
    arg = statHLookup heap argAddr
primNeg _ = error "Wrong stack for negate is detected"

numStep (output, [_], stack : dump, heap, globals, stats) _
  = (output, stack, dump, heap, globals, stats)
numStep (_, stack, _ : _, heap, _, _) _
  = error ("Wrong stack is detected : " ++ iDisplay (showStack heap stack))
numStep (_, _, dump, heap, _, _) _
  = error ("Wrong dump is detected : " ++ iDisplay (iInterleave iNewline (map (showStack heap) dump)))

apStep (output, stack@(topAddr : _), dump, heap, globals, stats) a1 a2
  = case arg of
      NInd a3 -> (output, stack, dump, makeHeap a3, globals, stats)
      _ -> (output, a1 : stack, dump, heap, globals, stats)
  where
    makeHeap = statHUpdate heap topAddr . NAp a1
    arg = statHLookup heap a2
apStep _ _ _ = error "Empty stack for application is dectected"

step state@(_, stack, _, heap, _, _)
  = dispatch (statHLookup heap (head stack))
  where
    dispatch (NNum n) = numStep state n
    dispatch (NAp a1 a2) = apStep state a1 a2
    dispatch (NSc scName argNames body)
      = tiStatIncScReds `applyToStats` scStep state scName argNames body
    dispatch (NInd addr) = indStep state addr
    dispatch (NPrim _ prim)
      = tiStatIncPReds `applyToStats` primStep state prim
    dispatch (NData tag args) = dataStep state tag args

dataStep (output, [_], stack : dump, heap, globals, stats) _ _
  = (output, stack, dump, heap, globals, stats)
dataStep (_, stack, _ : _, heap, _, _) _ _
  = error ("Wrong stack is detected : " ++ iDisplay (showStack heap stack))
dataStep (_, _, dump, heap, _, _) _ _
  = error ("Wrong dump is detected : " ++ iDisplay (iInterleave iNewline (map (showStack heap) dump)))

primConstr (output, stack, dump, heap, globals, stats) tag arity
  | length stack >= arity + 1 = (output, stack', dump, heap', globals, stats)
  | otherwise = error "Wrong stack for data type construction is detected"
  where
    stack'@(rootAddr : _) = drop arity stack
    heap' = statHUpdate heap rootAddr (NData tag args)
    args = take arity $ getArgs heap stack

primIf (output, stack@(_ : _ : _ : _ : _), dump, heap, globals, stats)
  = case cond of
      NData 1 [] -> (output, rootStack, dump, falseHeap, globals, stats)
      NData 2 [] -> (output, rootStack, dump, trueHeap, globals, stats)
      _
        | isDataNode cond -> error "Wrong data type for if is detected"
        | otherwise -> (output, [condAddr], ifApStack : dump, heap, globals, stats)
  where
    trueHeap = statHUpdate heap rootAddr (NInd trueAddr)
    falseHeap = statHUpdate heap rootAddr (NInd falseAddr)

    _ : ifApStack = stack
    _ : _ : rootStack = ifApStack
    rootAddr : _ = rootStack

    condAddr : trueAddr : falseAddr : _ = getArgs heap stack
    cond = statHLookup heap condAddr
primIf _ = error "Wrong stack for if is detected"

primDyadic (output, stack@(_ : _ : _ : _), dump, heap, globals, stats) f
  | arg1IsDataNode && arg2IsDataNode = (output, ap2Stack, dump, heap', globals, stats)
  | arg2IsDataNode = (output, [arg1Addr], ap1Stack : dump, heap, globals, stats)
  | otherwise = (output, [arg2Addr], ap2Stack : dump, heap, globals, stats)
  where
    heap' = statHUpdate heap rootAddr (f arg1 arg2)

    _ : ap1Stack = stack
    _ : ap2Stack = ap1Stack
    rootAddr : _ = ap2Stack

    arg1Addr : arg2Addr : _ = getArgs heap stack
    arg1 = statHLookup heap arg1Addr
    arg2 = statHLookup heap arg2Addr
    arg1IsDataNode = isDataNode arg1
    arg2IsDataNode = isDataNode arg2
primDyadic _ _ = error "Wrong stack for a binary operation is detected"

primCasePair (output, stack@(_ : _ : _ : _), dump, heap, globals, stats)
  = case expr of
      NData 1 [arg1, arg2] -> (output, rootStack, dump, makeHeap arg1 arg2, globals, stats)
      _
        | isDataNode expr -> error "Wrong data type for casePair is detected"
        | otherwise -> (output, [exprAddr], caseApStack : dump, heap, globals, stats)
  where
    makeHeap arg1 arg2 = statHUpdate heap' rootAddr (NAp funAddr' arg2)
      where
        (heap', funAddr') = statHAlloc heap (NAp funAddr arg1)

    _ : caseApStack = stack
    _ : rootStack = caseApStack
    rootAddr : _ = rootStack

    exprAddr : funAddr : _ = getArgs heap stack
    expr = statHLookup heap exprAddr
primCasePair _ = error "Wrong stack for casePair is detected"

primCaseList (output, stack@(_ : _ : _ : _ : _), dump, heap, globals, stats)
  = case expr of
      NData 1 [] -> (output, rootStack, dump, nilHeap, globals, stats)
      NData 2 [h, t] -> (output, rootStack, dump, makeConsHeap h t, globals, stats)
      _
        | isDataNode expr -> error "Wrong data type for caseList is detected"
        | otherwise -> (output, [exprAddr], caseApStack : dump, heap, globals, stats)
  where
    nilHeap = statHUpdate heap rootAddr (NInd nilAddr)
    makeConsHeap h t = statHUpdate heap' rootAddr (NAp consAddr' t)
      where
        (heap', consAddr') = statHAlloc heap (NAp consAddr h)

    _ : caseApStack = stack
    _ : _ : rootStack = caseApStack
    rootAddr : _ = rootStack

    exprAddr : nilAddr : consAddr : _ = getArgs heap stack
    expr = statHLookup heap exprAddr
primCaseList _ = error "Wrong stack for caseList is detected"

data Primitive
  = Neg
  | Add
  | Sub
  | Mul
  | Div
  | PrimConstr Int Int
  | If
  | Greater
  | GreaterEq
  | Less
  | LessEq
  | Eq
  | NotEq
  | CasePair
  | CaseList
  | Abort
  | Stop
  | Print

tiFinal (_, [soleAddr], [], heap, _, _) = isDataNode (statHLookup heap soleAddr)
tiFinal (_, [], _, _, _, _) = True
tiFinal _ = False

primStep state Neg = primNeg state
primStep state Add = primArith state (+)
primStep state Sub = primArith state (-)
primStep state Mul = primArith state (*)
primStep state Div = primArith state div
primStep state (PrimConstr tag arity) = primConstr state tag arity
primStep state If = primIf state
primStep state Greater = primComp state (>)
primStep state GreaterEq = primComp state (>=)
primStep state Less = primComp state (<)
primStep state LessEq = primComp state (<=)
primStep state Eq = primComp state (==)
primStep state NotEq = primComp state (/=)
primStep state CasePair = primCasePair state
primStep state CaseList = primCaseList state
primStep state Abort = error "Program is aborted by abort primitive"
primStep state Stop = primStop state
primStep state Print = primPrint state

primStop :: TiState -> TiState
primStop (output, [_], [], heap, globals, stats)
  = (output, [], [], heap, globals, stats)
primStop (_, _, [], _, _, _) = error "Wrong stack for stop is dectected"
primStop _ = error "Wrong dump for stop is dectected"

primPrint :: TiState -> TiState
primPrint (output, stack@[_, _, _], [], heap, globals, stats)
  = case arg1 of
      NNum v -> (v : output, [arg2Addr], [], heap, globals, stats)
      _
        | isDataNode arg1 -> error "Wrong data type for print is detected"
        | otherwise -> (output, [arg1Addr], [arg1Stack], heap, globals, stats)
  where
    _ : _ : arg1Stack = stack
    arg1Addr : arg2Addr : _ = getArgs heap stack
    arg1 = statHLookup heap arg1Addr
primPrint (_, _, [], _, _, _) = error "Wrong stack for print is dectected"
primPrint _ = error "Wrong dump for print is dectected"

primitives
  = [ ("negate", Neg)
    , ("+", Add), ("-", Sub)
    , ("*", Mul), ("/", Div)
    , ("if", If)
    , (">", Greater), (">=", GreaterEq)
    , ("<", Less), ("<=", LessEq)
    , ("==", Eq), ("~=", NotEq)
    , ("casePair", CasePair)
    , ("caseList", CaseList)
    , ("abort", Abort)
    , ("stop", Stop)
    , ("print", Print)
    ]

showResults states
  = iDisplay resultSeq
  where
    resultSeq
      = iConcat [ iLayn (map showState states)
                , showOutput (last states)
                , showStats (last states)
                ]

showOutput :: TiState -> ISeq
showOutput (output, _, _, _, _, _)
  = iConcat [ iStr "[", iInterleave (iStr ", ") . map iNum . reverse $ output, iStr "]"]

extraPreludeDefs
  = [ ("False", [], EConstr 1 0)
    , ("True", [], EConstr 2 0)
    , ("and", ["x", "y"], EAp (EAp (EAp (EVar "if") (EVar "x")) (EVar "y")) (EVar "False"))
    , ("or", ["x", "y"], EAp (EAp (EAp (EVar "if") (EVar "x")) (EVar "True")) (EVar "y"))
    , ("xor", ["x", "y"], EAp (EAp (EAp (EVar "if") (EVar "x")) (EAp (EVar "not") (EVar "y"))) (EVar "y"))
    , ("not", ["y"], EAp (EAp (EAp (EVar "if") (EVar "y")) (EVar "False")) (EVar "True"))
    , ("MkPair", [], EConstr 1 2)
    , ("fst", ["p"], EAp (EAp (EVar "casePair") (EVar "p")) (EVar "K"))
    , ("snd", ["p"], EAp (EAp (EVar "casePair") (EVar "p")) (EVar "K1"))
    , ("Cons", [], EConstr 2 2)
    , ("Nil", [], EConstr 1 0)
    , ("head", ["l"], EAp (EAp (EAp (EVar "caseList") (EVar "l")) (EVar "abort")) (EVar "K"))
    , ("tail", ["l"], EAp (EAp (EAp (EVar "caseList") (EVar "l")) (EVar "abort")) (EVar "K1"))
    , ("printList", ["xs"], EAp (EAp (EAp (EVar "caseList") (EVar "xs")) (EVar "stop")) (EVar "printCons"))
    , ("printCons", ["h", "t"], EAp (EAp (EVar "print") (EVar "h")) (EAp (EVar "printList") (EVar "t")))
    ]

compile program
  = ([], initialStack, initialTiDump, initialHeap, globals, tiStatInitial)
  where
    scDefs = program ++ preludeDefs ++ extraPreludeDefs

    (heap, globals) = buildInitialHeap scDefs
    (initialHeap, addressOfEntry) = statHAlloc heap (NAp addressOfPrintList addressOfMain)
    initialStack = [addressOfEntry]

    addressOfMain = aLookup globals "main" (error "main is not defined")
    addressOfPrintList = aLookup globals "printList" (error "printList is not defined")
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
#endif
#endif
#endif
