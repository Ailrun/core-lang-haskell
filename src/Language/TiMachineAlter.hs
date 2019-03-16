{-# LANGUAGE CPP #-}
module Language.TiMachineAlter where

import Data.ISeq
import Data.List
import Data.StatHeap
import Language.Parser
import Language.Prelude
import Language.Types
import Util

run :: String -> String
run = showResults . eval . compile . parse

compile :: CoreProgram -> TiState
compile program
  = ([], initialStack, initialTiDump, initialHeap, globals, tiStatInitial)
  where
    scDefs = program ++ preludeDefs ++ extraPreludeDefs

    (heap, globals) = buildInitialHeap scDefs
    (initialHeap, addressOfEntry) = statHAlloc heap (NAp addressOfPrintList addressOfMain)
    initialStack = [addressOfEntry]

    addressOfMain = aLookup globals "main" (error "main is not defined")
    addressOfPrintList = aLookup globals "printList" (error "printList is not defined")

eval :: TiState -> [TiState]
eval state
  = state : restStates
  where
    restStates
      | tiFinal state = []
      | otherwise = eval nextState
    nextState = doAdmin (step state)

showResults :: [TiState] -> String
showResults states
  = iDisplay resultSeq
  where
    resultSeq
      = iConcat [ iLayn (map showState states)
                , showOutput (last states)
                , showStats (last states)
                ]

type TiState = (TiOutput, TiStack, TiDump, TiHeap, TiGlobals, TiStats)

type TiOutput = [Int]

type TiStack = [Addr]

#if __CLH_EXERCISE_2__ < 28
type TiDump = [TiStack]

initialTiDump :: TiDump
initialTiDump = []
#endif

type TiHeap = StatHeap Node

type TiGlobals = Assoc Name Addr

type TiStats
  = ( Int -- The number of steps
    , ( Int -- The number of supercombinator reduction
      , Int -- The number of primitive reduction
      )
    , Int -- The maximun stack depth
    )

tiStatInitial :: TiStats
tiStatInitial = (0, (0, 0), 0)
tiStatIncSteps :: TiStats -> TiStats
tiStatIncSteps (steps, redStats, maxStackDepth)
  = (steps + 1, redStats, maxStackDepth)
tiStatGetSteps :: TiStats -> Int
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

applyToStats :: (TiStats -> TiStats) -> TiState -> TiState
applyToStats statFun (output, stack, dump, heap, scDefs, stats)
  = (output, stack, dump, heap, scDefs, statFun stats)

extraPreludeDefs :: CoreProgram
#if __CLH_EXERCISE_2__ != 29
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
#endif

buildInitialHeap :: [CoreScDefn] -> (TiHeap, TiGlobals)
buildInitialHeap scDefs
  = (heap2, scAddrs ++ primAddrs)
  where
    (heap1, scAddrs) = mapAccumL allocateSc statHInitial scDefs
    (heap2, primAddrs) = mapAccumL allocatePrim heap1 primitives

allocateSc :: TiHeap -> CoreScDefn -> (TiHeap, (Name, Addr))
allocateSc heap (name, args, body)
  = (heap', (name, addr))
  where
    (heap', addr) = statHAlloc heap (NSc name args body)

doAdmin :: TiState -> TiState
doAdmin state@(_, stack, _, _, _, stats)
  = applyToStats (updateMaxStackDepth . tiStatIncSteps) state
  where
    updateMaxStackDepth
      | stackDepth <= statMaxStackDepth = id
      | otherwise = tiStatSetMaxStackDepth stackDepth

    stackDepth = length stack
    statMaxStackDepth = tiStatGetMaxStackDepth stats

tiFinal :: TiState -> Bool
tiFinal (_, [soleAddr], [], heap, _, _) = isDataNode (statHLookup heap soleAddr)
tiFinal (_, [], _, _, _, _) = True
tiFinal _ = False

isDataNode :: Node -> Bool
isDataNode (NNum n) = True
isDataNode (NData tag args) = True
isDataNode node = False

step :: TiState -> TiState
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

numStep :: TiState -> Int -> TiState
#if __CLH_EXERCISE_2__ < 28
numStep (output, [_], stack : dump, heap, globals, stats) _
  = (output, stack, dump, heap, globals, stats)
numStep (_, stack, _ : _, heap, _, _) _
  = error ("Wrong stack is detected : " ++ iDisplay (showStack heap stack))
numStep (_, _, dump, heap, _, _) _
  = error ("Wrong dump is detected : " ++ iDisplay (iInterleave iNewline (map (showStack heap) dump)))
#endif

apStep :: TiState -> Addr -> Addr -> TiState
apStep (output, stack@(topAddr : _), dump, heap, globals, stats) a1 a2
  = case arg of
      NInd a3 -> (output, stack, dump, makeHeap a3, globals, stats)
      _ -> (output, a1 : stack, dump, heap, globals, stats)
  where
    makeHeap = statHUpdate heap topAddr . NAp a1
    arg = statHLookup heap a2
apStep _ _ _ = error "Empty stack for application is dectected"

scStep :: TiState -> Name -> [Name] -> CoreExpr -> TiState
#if __CLH_EXERCISE_2__ < 28
scStep (output, stack, dump, heap, globals, stats) scName argNames body
  | argsLength + 1 <= length stack = (output, stack', dump, heap', globals, stats)
  | otherwise = error ("Two few arguments are provided to the function " ++ scName)
  where
    stack'@(rootAddr : _) = drop argsLength stack
    heap' = instantiateAndUpdate body rootAddr heap env
    env = argBindings ++ globals
    argBindings = zip argNames (getArgs heap stack)
    argsLength = length argNames
#endif

getArgs :: TiHeap -> TiStack -> [Addr]
getArgs heap (_ : stack)
  = map getArg stack
  where
    getArg a
      = case statHLookup heap a of
          NAp _ arg -> arg
          _ -> error "Cannot get arg from non-application node"
getArgs _ _ = error "Cannot get args from empty stack"

instantiate :: CoreExpr -> TiHeap -> TiGlobals -> (TiHeap, Addr)
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

instantiateConstr :: Int -> Int -> TiHeap -> TiGlobals -> (TiHeap, Addr)
#if __CLH_EXERCISE_2__ < 27
instantiateConstr tag arity heap env = (heap', addr)
  where
    (heap', addr) = statHAlloc heap (NPrim "Pack" (PrimConstr tag arity))
#endif

instantiateLet :: IsRec -> Assoc Name CoreExpr -> CoreExpr -> TiHeap -> TiGlobals -> (TiHeap, Addr)
instantiateLet isRec defs body heap env = instantiate body heap' env'
  where
    (heap', defBindings) = mapAccumL allocateDef heap defs
    allocateDef = instantiateDef (if isRec then env' else env)
    env' = defBindings ++ env

instantiateDef :: TiGlobals -> TiHeap -> (Name, CoreExpr) -> (TiHeap, (Name, Addr))
instantiateDef env heap (name, body)
  = (heap', (name, addr))
  where
    (heap', addr) = instantiate body heap env

showState :: TiState -> ISeq
showState (_, stack, _, heap, _, _)
  = iConcat [ showStack heap stack, iNewline
            , iStr "Heap Size: ", iNum (statHSize heap), iNewline
            ]

showStack :: TiHeap -> TiStack -> ISeq
showStack heap stack
  = iConcat [ iStr "Stk ["
            , iIndent (iInterleave iNewline (map showStackItem stack))
            , iStr "]"
            ]
  where
    showStackItem addr
      = iConcat [ showFWAddr addr, iStr ": ", showStkNode heap (statHLookup heap addr) ]

showStkNode :: TiHeap -> Node -> ISeq
showStkNode heap (NAp funAddr argAddr)
  = iConcat [ iStr "NAp ", showFWAddr funAddr, iStr " ", showFWAddr argAddr
            , iStr " (", showNode heap (statHLookup heap argAddr), iStr ")"
            ]
showStkNode heap node = showNode heap node

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

showHeap :: TiHeap -> ISeq
showHeap heap
  = iConcat [ iStr "Heap ["
            , iIndent (iInterleave iNewline (map showHeapItem (statHAddresses heap)))
            , iStr "]"
            ]
    where
      showHeapItem addr
        = iConcat [ showFWAddr addr, iStr ": ", showStkNode heap (statHLookup heap addr) ]

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

showNode :: TiHeap -> Node -> ISeq
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

indStep :: TiState -> Addr -> TiState
indStep (output, _ : stack, dump, heap, globals, stats) addr
  = (output, addr : stack, dump, heap, globals, stats)
indStep _ _ = error "Wrong stack!"

instantiateAndUpdate :: CoreExpr -> Addr -> TiHeap -> TiGlobals -> TiHeap
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
  = error "Can't instantiate case exprs"

instantiateAndUpdateConstr :: Int -> Int -> Addr -> TiHeap -> TiGlobals -> TiHeap
#if __CLH_EXERCISE_2__ < 27
instantiateAndUpdateConstr tag arity addr heap env = heap'
  where
    heap' = statHUpdate heap addr (NPrim "Pack" (PrimConstr tag arity))
#endif

instantiateAndUpdateLet :: IsRec -> Assoc Name CoreExpr -> CoreExpr -> Addr -> TiHeap -> TiGlobals -> TiHeap
instantiateAndUpdateLet isRec defs body addr heap env = instantiateAndUpdate body addr heap' env'
  where
    (heap', defBindings) = mapAccumL allocateDef heap defs
    allocateDef = instantiateDef (if isRec then env' else env)
    env' = defBindings ++ env

#if __CLH_EXERCISE_2__ < 27
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
#endif

primitives :: Assoc Name Primitive
#if __CLH_EXERCISE_2__ < 27
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
#endif

allocatePrim :: TiHeap -> (Name, Primitive) -> (TiHeap, (Name, Addr))
allocatePrim heap (name, prim) = (heap', (name, addr))
  where
    (heap', addr) = statHAlloc heap (NPrim name prim)

primStep :: TiState -> Primitive -> TiState
#if __CLH_EXERCISE_2__ < 27
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
#endif

-- Do we need to check stack length?
-- It should be longer than or equal to 2
primNeg :: TiState -> TiState
#if __CLH_EXERCISE_2__ < 28
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
#endif

primArith :: TiState -> (Int -> Int -> Int) -> TiState
primArith state f = primDyadic state nodeF
  where
    nodeF (NNum v1) (NNum v2) = NNum (f v1 v2)
    nodeF _ _ = error "Wrong data type for a binary arithmetic operation is detected"

data Node
  = NAp Addr Addr
  | NSc Name [Name] CoreExpr
  | NNum Int
  | NInd Addr
  | NPrim Name Primitive
  | NData Int [Addr]

dataStep :: TiState -> Int -> [Addr] -> TiState
#if __CLH_EXERCISE_2__ < 28
dataStep (output, [_], stack : dump, heap, globals, stats) _ _
  = (output, stack, dump, heap, globals, stats)
dataStep (_, stack, _ : _, heap, _, _) _ _
  = error ("Wrong stack is detected : " ++ iDisplay (showStack heap stack))
dataStep (_, _, dump, heap, _, _) _ _
  = error ("Wrong dump is detected : " ++ iDisplay (iInterleave iNewline (map (showStack heap) dump)))
#endif

primConstr :: TiState -> Int -> Int -> TiState
#if __CLH_EXERCISE_2__ < 28
primConstr (output, stack, dump, heap, globals, stats) tag arity
  | length stack >= arity + 1 = (output, stack', dump, heap', globals, stats)
  | otherwise = error "Wrong stack for data type construction is detected"
  where
    stack'@(rootAddr : _) = drop arity stack
    heap' = statHUpdate heap rootAddr (NData tag args)
    args = take arity $ getArgs heap stack
#endif

primIf :: TiState -> TiState
#if __CLH_EXERCISE_2__ < 28
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
#endif

primComp :: TiState -> (Int -> Int -> Bool) -> TiState
primComp state f = primDyadic state nodeF
  where
    nodeF (NNum v1) (NNum v2)
      | f v1 v2 = NData 2 []
      | otherwise = NData 1 []
    nodeF _ _ = error "Wrong data type for a binary comparison operation is detected"

primDyadic :: TiState -> (Node -> Node -> Node) -> TiState
#if __CLH_EXERCISE_2__ < 28
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
#endif

primCasePair :: TiState -> TiState
#if __CLH_EXERCISE_2__ < 28
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
#endif

primCaseList :: TiState -> TiState
#if __CLH_EXERCISE_2__ < 28
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
#endif

primStop :: TiState -> TiState
primStop (output, [_], [], heap, globals, stats)
  = (output, [], [], heap, globals, stats)
primStop (_, _, [], _, _, _) = error "Wrong stack for stop is dectected"
primStop _ = error "Wrong dump for stop is dectected"

primPrint :: TiState -> TiState
#if __CLH_EXERCISE_2__ < 28
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
#endif

showOutput :: TiState -> ISeq
showOutput (output, _, _, _, _, _)
  = iConcat [ iStr "[", iInterleave (iStr ", ") . map iNum . reverse $ output, iStr "]"]

#if __CLH_EXERCISE_2__ >= 27
type Primitive = TiState -> TiState

#if __CLH_EXERCISE_2__ != 29
primitives
  = [ ("negate", primNeg)
    , ("+", (flip primArith (+))), ("-", (flip primArith (-)))
    , ("*", (flip primArith (*))), ("/", (flip primArith div))
    , ("if", primIf)
    , (">", (flip primComp (>))), (">=", (flip primComp (>=)))
    , ("<", (flip primComp (<))), ("<=", (flip primComp (<=)))
    , ("==", (flip primComp (==))), ("~=", (flip primComp (/=)))
    , ("casePair", primCasePair)
    , ("caseList", primCaseList)
    , ("abort", error "Program is aborted by abort primitive")
    , ("stop", primStop)
    , ("print", primPrint)
    ]
#endif

primStep state prim = prim state

instantiateConstr tag arity heap env = (heap', addr)
  where
    (heap', addr) = statHAlloc heap (NPrim "Pack" (\state -> primConstr state tag arity))

instantiateAndUpdateConstr tag arity addr heap env = heap'
  where
    heap' = statHUpdate heap addr (NPrim "Pack" (\state -> primConstr state tag arity))

#if __CLH_EXERCISE_2__ >= 28
type TiDump = [Int]

initialTiDump :: TiDump
initialTiDump = []

getCurrentStackLength :: TiStack -> TiDump -> Int
getCurrentStackLength stack dump = length stack - lastDumpPoint
  where
    lastDumpPoint = case dump of
      [] -> 0
      p : _ -> p

numStep (output, stack, stackPoint : dump, heap, globals, stats) _
  | stackLength == stackPoint + 1 = (output, stack', dump, heap, globals, stats)
  | otherwise = error ("Wrong stack is detected : " ++ iDisplay (showStack heap stack))
  where
    stack' = drop 1 stack

    stackLength = length stack
numStep (_, _, dump, heap, _, _) _
  = error ("Wrong dump is detected : " ++ iDisplay (iInterleave (iStr ", ") (map iNum dump)))

primNeg (output, stack, dump, heap, globals, stats)
  | currentStackLength >= 2 = case arg of
      NNum v -> (output, negApStack, dump, makeHeap v, globals, stats)
      _
        | isDataNode arg -> error "Negation cannot be applied to other than numbers"
        | otherwise -> (output, argAddr : negApStack, (stackLength - 1) : dump, heap, globals, stats);
  | otherwise = error "Wrong stack for negate is detected"
  where
    _ : negApStack@(rootAddr : _) = stack

    makeHeap = statHUpdate heap rootAddr . NNum . negate

    argAddr : _ = getArgs heap stack
    arg = statHLookup heap argAddr

    stackLength = length stack
    currentStackLength = getCurrentStackLength stack dump

dataStep (output, stack, stackPoint : dump, heap, globals, stats) _ _
  | stackLength == stackPoint + 1 = (output, stack', dump, heap, globals, stats)
  | otherwise = error ("Wrong stack is detected : " ++ iDisplay (showStack heap stack))
  where
    stack' = drop 1 stack

    stackLength = length stack
dataStep (_, _, dump, heap, _, _) _ _
  = error ("Wrong dump is detected : " ++ iDisplay (iInterleave (iStr ", ") (map iNum dump)))

primIf (output, stack, dump, heap, globals, stats)
  | currentStackLength >= 4 = case cond of
      NData 1 [] -> (output, rootStack, dump, falseHeap, globals, stats)
      NData 2 [] -> (output, rootStack, dump, trueHeap, globals, stats)
      _
        | isDataNode cond -> error "Wrong data type for if is detected"
        | otherwise -> (output, condAddr : ifApStack, (stackLength - 1) : dump, heap, globals, stats);
  | otherwise = error "Wrong stack for if is detected"
  where
    trueHeap = statHUpdate heap rootAddr (NInd trueAddr)
    falseHeap = statHUpdate heap rootAddr (NInd falseAddr)

    _ : ifApStack = stack
    _ : _ : rootStack = ifApStack
    rootAddr : _ = rootStack

    condAddr : trueAddr : falseAddr : _ = getArgs heap stack
    cond = statHLookup heap condAddr

    stackLength = length stack
    currentStackLength = getCurrentStackLength stack dump

primDyadic (output, stack, dump, heap, globals, stats) f
  | currentStackLength >= 3 =
    case (arg1IsDataNode, arg2IsDataNode) of
      (True, True) -> (output, ap2Stack, dump, heap', globals, stats)
      (_, True) -> (output, arg1Addr : ap1Stack, (stackLength - 1) : dump, heap, globals, stats)
      _ -> (output, arg2Addr : ap2Stack, (stackLength - 2) : dump, heap, globals, stats)
  | otherwise = error "Wrong stack for a binary operation is detected"
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

    stackLength = length stack
    currentStackLength = getCurrentStackLength stack dump

primCasePair (output, stack, dump, heap, globals, stats)
  | currentStackLength >= 3 = case expr of
      NData 1 [arg1, arg2] -> (output, rootStack, dump, makeHeap arg1 arg2, globals, stats)
      _
        | isDataNode expr -> error "Wrong data type for casePair is detected"
        | otherwise -> (output, exprAddr : caseApStack, (stackLength - 1) : dump, heap, globals, stats);
  | otherwise = error "Wrong stack for casePair is detected"
  where
    makeHeap arg1 arg2 = statHUpdate heap' rootAddr (NAp funAddr' arg2)
      where
        (heap', funAddr') = statHAlloc heap (NAp funAddr arg1)

    _ : caseApStack = stack
    _ : rootStack = caseApStack
    rootAddr : _ = rootStack

    exprAddr : funAddr : _ = getArgs heap stack
    expr = statHLookup heap exprAddr

    stackLength = length stack
    currentStackLength = getCurrentStackLength stack dump

primCaseList (output, stack, dump, heap, globals, stats)
  | currentStackLength >= 4 = case expr of
      NData 1 [] -> (output, rootStack, dump, nilHeap, globals, stats)
      NData 2 [h, t] -> (output, rootStack, dump, makeConsHeap h t, globals, stats)
      _
        | isDataNode expr -> error "Wrong data type for caseList is detected"
        | otherwise -> (output, exprAddr : caseApStack, (stackLength - 1) : dump, heap, globals, stats);
  | otherwise = error "Wrong stack for caseList is detected"
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

    stackLength = length stack
    currentStackLength = getCurrentStackLength stack dump

primPrint (output, stack, [], heap, globals, stats)
  | stackLength == 3 = case arg1 of
      NNum v -> (v : output, [arg2Addr], [], heap, globals, stats)
      _
        | isDataNode arg1 -> error "Wrong data type for print is detected"
        | otherwise -> (output, arg1Addr :arg1Stack, [1], heap, globals, stats);
  | otherwise = error "Wrong stack for print is dectected"
  where
    _ : _ : arg1Stack = stack
    arg1Addr : arg2Addr : _ = getArgs heap stack
    arg1 = statHLookup heap arg1Addr

    stackLength = length stack
primPrint _ = error "Wrong dump for print is dectected"

scStep (output, stack, dump, heap, globals, stats) scName argNames body
  | argsLength + 1 <= currentStackLength = (output, stack', dump, heap', globals, stats)
  | otherwise = error ("Two few arguments are provided to the function " ++ scName)
  where
    stack'@(rootAddr : _) = drop argsLength stack
    heap' = instantiateAndUpdate body rootAddr heap env
    env = argBindings ++ globals
    argBindings = zip argNames (getArgs heap stack)
    argsLength = length argNames

    stackLength = length stack
    currentStackLength = getCurrentStackLength stack dump

primConstr (output, stack, dump, heap, globals, stats) tag arity
  | currentStackLength >= arity + 1 = (output, stack', dump, heap', globals, stats)
  | otherwise = error "Wrong stack for data type construction is detected"
  where
    stack'@(rootAddr : _) = drop arity stack
    heap' = statHUpdate heap rootAddr (NData tag args)
    args = take arity $ getArgs heap stack

    stackLength = length stack
    currentStackLength = getCurrentStackLength stack dump

#if __CLH_EXERCISE_2__ == 29
extraPreludeDefs
  = [ ("False", ["t", "f"], EVar "f")
    , ("True", ["t", "f"], EVar "t")
    , ("if", [], EVar "I")
    , ("and", ["b1", "b2", "t", "f"], EAp (EAp (EVar "b1") (EAp (EAp (EVar "b2") (EVar "t")) (EVar "f"))) (EVar "f"))
    , ("or", ["b1", "b2", "t", "f"], EAp (EAp (EVar "b1") (EVar "t")) (EAp (EAp (EVar "b2") (EVar "t")) (EVar "f")))
    , ("xor", ["b1", "b2", "t", "f"], EAp (EAp (EVar "b1") (EAp (EAp (EVar "b2") (EVar "f")) (EVar "t"))) (EAp (EAp (EVar "b2") (EVar "t")) (EVar "f")))
    , ("not", ["b", "t", "f"], EAp (EAp (EVar "b") (EVar "f")) (EVar "t"))
    , ("pair", ["a", "b", "f"], EAp (EAp (EVar "f") (EVar "a")) (EVar "b"))
    , ("casePair", [], EVar "I")
    , ("fst", ["p"], EAp (EVar "p") (EVar "K"))
    , ("snd", ["p"], EAp (EVar "p") (EVar "K1"))
    , ("cons", ["a", "b", "cn", "cc"], EAp (EAp (EVar "cc") (EVar "a")) (EVar "b"))
    , ("nil", ["cn", "cc"], EVar "cn")
    , ("caseList", [], EVar "I")
    , ("head", ["l"], EAp (EAp (EAp (EVar "caseList") (EVar "l")) (EVar "abort")) (EVar "K"))
    , ("tail", ["l"], EAp (EAp (EAp (EVar "caseList") (EVar "l")) (EVar "abort")) (EVar "K1"))
    , ("printList", ["xs"], EAp (EAp (EAp (EVar "caseList") (EVar "xs")) (EVar "stop")) (EVar "printCons"))
    , ("printCons", ["h", "t"], EAp (EAp (EVar "print") (EVar "h")) (EAp (EVar "printList") (EVar "t")))
    ]

primitives
  = [ ("negate", primNeg)
    , ("+", (flip primArith (+))), ("-", (flip primArith (-)))
    , ("*", (flip primArith (*))), ("/", (flip primArith div))
    , (">", (flip primComp (>))), (">=", (flip primComp (>=)))
    , ("<", (flip primComp (<))), ("<=", (flip primComp (<=)))
    , ("==", (flip primComp (==))), ("~=", (flip primComp (/=)))
    , ("abort", error "Program is aborted by abort primitive")
    , ("stop", primStop)
    , ("print", primPrint)
    ]
#endif
#endif
#endif
