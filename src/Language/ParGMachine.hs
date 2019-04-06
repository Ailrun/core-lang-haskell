{-# LANGUAGE CPP #-}
module Language.ParGMachine
  ( parGMRun
  , run
#if __CLH_EXERCISE_5__ >= 2
  , eval
#if __CLH_EXERCISE_5__ >= 3
  , compile
#if __CLH_EXERCISE_5__ >= 5
  , showResults
#endif
#endif
#endif
  )
where

import Control.Arrow
import Data.List
import Data.ISeq
import Language.Parser
import Language.Prelude
import Language.Types
import Util

parGMRun = putStrLn . run

run :: String -> String
#if __CLH_EXERCISE_5__ >= 5
run = showResults . eval . compile . parse
#else
run = undefined
#endif

#if __CLH_EXERCISE_5__ >= 1
type PgmState = (PgmGlobalState, [PgmLocalState])

type PgmGlobalState = (GmOutput, GmHeap, GmGlobals, GmSparks, GmStats)

type GmOutput = String

pgmGetOutput :: PgmState -> GmOutput
pgmGetOutput ((output, _, _, _, _), _) = output

type GmHeap = Heap Node
#if __CLH_EXERCISE_5__ < 8
data Node
  = NNum Int
  | NAp Addr Addr
  | NGlobal Int GmCode
  | NInd Addr
  | NConstr Int [Addr]
  deriving (Show, Read, Eq)
#endif

pgmGetHeap :: PgmState -> GmHeap
pgmGetHeap ((_, heap, _, _, _), _) = heap

type GmGlobals = Assoc Name Addr

pgmGetGlobals :: PgmState -> GmGlobals
pgmGetGlobals ((_, _, globals, _, _), _) = globals

#if __CLH_EXERCISE_5__ < 18
type GmSparks = [Addr]
#endif

pgmGetSparks :: PgmState -> GmSparks
pgmGetSparks ((_, _, _, sparks, _), _) = sparks

type GmStats = [Int]

pgmGetStats :: PgmState -> GmStats
pgmGetStats ((_, _, _, _, stats), _) = stats

type PgmLocalState = (GmCode, GmStack, GmDump, GmVStack, GmClock)

type GmCode = [Instruction]
#if __CLH_EXERCISE_5__ < 2
data Instruction
  = Push Int
  | PushGlobal GlobalMode
  | PushInt Int
  | PushBasic Int
  | Pop Int
  | Slide Int
  | Update Int
  | UpdateInt Int
  | UpdateBool Int
  | Alloc Int
  | Pack Int Int
  | Split Int
  | CaseJump (Assoc Int GmCode)
  | Cond GmCode GmCode
  | MkAp
  | MkInt
  | MkBool
  | Get
  | Add | Sub | Mul | Div
  | Neg
  | Eq | Ne | Lt | Le | Gt | Ge
  | Eval
  | Return
  | Unwind
  | Print
  deriving (Show, Read, Eq)
#endif
data GlobalMode
  = GlobalLabel Name
  | GlobalPack Int Int
  deriving (Show, Read, Eq)

type GmStack = [Addr]

type GmDump = [GmDumpItem]
type GmDumpItem = (GmCode, GmStack, GmVStack)

type GmVStack = [Int]

type GmClock = Int

type GmState = (PgmGlobalState, PgmLocalState)

putOutput :: GmOutput -> GmState -> GmState
putHeap :: GmHeap -> GmState -> GmState
putGlobals :: GmGlobals -> GmState -> GmState
putSparks :: GmSparks -> GmState -> GmState
putStats :: GmStats -> GmState -> GmState

getOutput :: GmState -> GmOutput
getHeap :: GmState -> GmHeap
getGlobals :: GmState -> GmGlobals
getSparks :: GmState -> GmSparks
getStats :: GmState -> GmStats

putCode :: GmCode -> GmState -> GmState
putStack :: GmStack -> GmState -> GmState
putDump :: GmDump -> GmState -> GmState
putVStack :: GmVStack -> GmState -> GmState
putClock :: GmClock -> GmState -> GmState

getCode :: GmState -> GmCode
getStack :: GmState -> GmStack
getDump :: GmState -> GmDump
getVStack :: GmState -> GmVStack
getClock :: GmState -> GmClock

putOutput output ((_, heap, globals, sparks, stats), locals) = ((output, heap, globals, sparks, stats), locals)
putHeap heap ((output, _, globals, sparks, stats), locals) = ((output, heap, globals, sparks, stats), locals)
putGlobals globals ((output, heap, _, sparks, stats), locals) = ((output, heap, globals, sparks, stats), locals)
putSparks sparks ((output, heap, globals, _, stats), locals) = ((output, heap, globals, sparks, stats), locals)
putStats stats ((output, heap, globals, sparks, _), locals) = ((output, heap, globals, sparks, stats), locals)

getOutput ((output, _, _, _, _), _) = output
getHeap ((_, heap, _, _, _), _) = heap
getGlobals ((_, _, globals, _, _), _) = globals
getSparks ((_, _, _, sparks, _), _) = sparks
getStats ((_, _, _, _, stats), _) = stats

putCode code (global, (_, stack, dump, vStack, clock)) = (global, (code, stack, dump, vStack, clock))
putStack stack (global, (code, _, dump, vStack, clock)) = (global, (code, stack, dump, vStack, clock))
putDump dump (global, (code, stack, _, vStack, clock)) = (global, (code, stack, dump, vStack, clock))
putVStack vStack (global, (code, stack, dump, _, clock)) = (global, (code, stack, dump, vStack, clock))
putClock clock (global, (code, stack, dump, vStack, _)) = (global, (code, stack, dump, vStack, clock))

getCode (_, (code, _, _, _, _)) = code
getStack (_, (_, stack, _, _, _)) = stack
getDump (_, (_, _, dump, _, _)) =  dump
getVStack (_, (_, _, _, vStack, _)) = vStack
getClock (_, (_, _, _, _, clock)) = clock

#if __CLH_EXERCISE_5__ >= 2
eval :: PgmState -> [PgmState]
eval state = state : restStates
  where
    restStates
      | gmFinal state = []
      | otherwise = eval (doAdmin (steps state))

steps :: PgmState -> PgmState
#if __CLH_EXERCISE_5__ < 13
steps state
  = mapAccumL step global' locals'
  where
    ((output, heap, globals, sparks, stats), locals) = state
    newTasks = [ makeTask s | s <- sparks ]
    global' = (output, heap, globals, [], stats)
    locals' = map tick (locals ++ newTasks)
#endif

makeTask :: Addr -> PgmLocalState
makeTask addr = ([Eval], [addr], [], [], 0)

tick :: PgmLocalState -> PgmLocalState
tick (code, stack, dump, vStack, clock) = (code, stack, dump, vStack, clock + 1)

gmFinal :: PgmState -> Bool
gmFinal = (uncurry (&&)) . (null . snd &&& null . pgmGetSparks)

step :: PgmGlobalState -> PgmLocalState -> GmState
step global locals = dispatch i (putCode is state)
  where
    i : is = getCode state
    state = (global, locals)

doAdmin :: PgmState -> PgmState
#if __CLH_EXERCISE_5__ < 23
doAdmin ((output, heap, globals, sparks, stats), locals)
  = ((output, heap, globals, sparks, stats'), locals')
  where
    (locals', stats') = foldr makeNewLocalAndStats ([], stats) locals

    makeNewLocalAndStats local@(code, _, _, _, clock) (localsAcc, statsAcc)
      | null code = (localsAcc, clock : statsAcc)
      | otherwise = (local : localsAcc, statsAcc)
#endif

data Instruction
  = Push Int
  | PushGlobal GlobalMode
  | PushInt Int
  | PushBasic Int
  | Pop Int
  | Slide Int
  | Update Int
  | UpdateInt Int
  | UpdateBool Int
  | Alloc Int
  | Pack Int Int
  | Split Int
  | CaseJump (Assoc Int GmCode)
  | Cond GmCode GmCode
  | MkAp
  | MkInt
  | MkBool
  | Get
  | Add | Sub | Mul | Div
  | Neg
  | Eq | Ne | Lt | Le | Gt | Ge
  | Eval
  | Return
  | Unwind
  | Par
  | Print
  deriving (Show, Read, Eq)

dispatch :: Instruction -> GmState -> GmState
dispatch (Push n) = push n
dispatch (PushGlobal f) = pushGlobal f
dispatch (PushInt n) = pushInt n
dispatch (PushBasic n) = pushBasic n
dispatch (Pop n) = pop n
dispatch (Slide n) = slide n
dispatch (Update n) = update n
dispatch (UpdateInt n) = updateInt n
dispatch (UpdateBool n) = updateBool n
dispatch (Alloc n) = alloc n
dispatch (Pack tag arity) = pack tag arity
dispatch (Split arity) = split arity
dispatch (CaseJump alters) = caseJump alters
dispatch (Cond c1 c2) = cond c1 c2
dispatch MkAp = mkAp
dispatch MkInt = mkInt
dispatch MkBool = mkBool
dispatch Get = getInstruction
dispatch Add = dyadicIntOp (+)
dispatch Sub = dyadicIntOp (-)
dispatch Mul = dyadicIntOp (*)
dispatch Div = dyadicIntOp div
dispatch Neg = neg
dispatch Eq = dyadicBoolOp (==)
dispatch Ne = dyadicBoolOp (/=)
dispatch Lt = dyadicBoolOp (<)
dispatch Le = dyadicBoolOp (<=)
dispatch Gt = dyadicBoolOp (>)
dispatch Ge = dyadicBoolOp (>=)
dispatch Eval = evalInstruction
dispatch Return = returnInstruction
dispatch Unwind = unwind
dispatch Par = parInstruction
dispatch Print = printInstruction

push :: Int -> GmState -> GmState
push n state = putStack (a : as) state
  where
    as = getStack state
    a = as !! n

pushGlobal :: GlobalMode -> GmState -> GmState
pushGlobal (GlobalLabel name) state
  = putStack (a : getStack state) state
  where
    a = aLookup (getGlobals state) name (error ("Undeclared global " ++ name))
pushGlobal (GlobalPack tag arity) state
  | not globalExists = putHeap heap' (putGlobals globals' (putStack stack' state))
  | otherwise = pushGlobal (GlobalLabel globalLabel) state
  where
    stack' = a : getStack state
    globals' = (globalLabel, a) : getGlobals state
    (heap', a) = hAlloc (getHeap state) (NGlobal arity [Pack tag arity, Update 0, Unwind])

    globalExists = globalLabel `elem` aDomain (getGlobals state)
    globalLabel = "Pack{" ++ show tag ++ "," ++ show arity ++ "}"

pushInt :: Int -> GmState -> GmState
pushInt n state
  | not (hIsNull a) = putStack stack' state
  where
    stack' = a : getStack state
    a = aLookup (getGlobals state) (show n) hNull
pushInt n state
  = putHeap heap' (putGlobals global' (putStack stack' state))
  where
    stack' = a : getStack state
    global' = (show n, a) : getGlobals state
    (heap', a) = hAlloc (getHeap state) (NNum n)

pushBasic :: Int -> GmState -> GmState
pushBasic n state = putVStack (n : getVStack state) state

pop :: Int -> GmState -> GmState
pop n state = putStack (drop n (getStack state)) state

slide :: Int -> GmState -> GmState
slide n state = putStack (a : drop n as) state
  where
    a : as = getStack state

update :: Int -> GmState -> GmState
#if __CLH_EXERCISE_5__ < 9
update n state = putStack stack' (putHeap heap' state)
  where
    heap' = hUpdate (getHeap state) rA (NInd a)
    rA = stack' !! n
    a : stack' = getStack state
#endif

updateInt :: Int -> GmState -> GmState
updateInt n state = putVStack vStack' (putHeap heap' state)
  where
    heap' = hUpdate (getHeap state) (getStack state !! n) (NNum v)
    v : vStack' = getVStack state

updateBool :: Int -> GmState -> GmState
updateBool n state = putVStack vStack' (putHeap heap' state)
  where
    heap' = hUpdate (getHeap state) (getStack state !! n) (NConstr v [])
    v : vStack' = getVStack state

alloc :: Int -> GmState -> GmState
alloc n state = putHeap heap' (putStack stack' state)
  where
    stack' = as ++ getStack state
    (heap', as) = allocNodes n (getHeap state)

allocNodes :: Int -> GmHeap -> (GmHeap, [Addr])
allocNodes 0 heap = (heap, [])
allocNodes n heap = (heap'', a : as)
  where
    (heap', as) = allocNodes (n - 1) heap
    (heap'', a) = hAlloc heap' (NInd hNull)

pack :: Int -> Int -> GmState -> GmState
pack tag arity state = putHeap heap' (putStack stack' state)
  where
    stack' = a : drop arity stack
    (heap', a) = hAlloc (getHeap state) (NConstr tag args)
    args = take arity stack
    stack = getStack state

split :: Int -> GmState -> GmState
split n state = putStack stack' state
  where
    stack' = args ++ as
    NConstr _ args = hLookup (getHeap state) a
    a : as = getStack state

caseJump :: Assoc Int GmCode -> GmState -> GmState
caseJump alters state = putCode code' state
  where
    code' = c' ++ getCode state
    c' = aLookup alters tag (error ("No case for constructor " ++ show tag))
    NConstr tag _ = hLookup (getHeap state) a
    a : _ = getStack state

cond :: GmCode -> GmCode -> GmState -> GmState
cond c1 c2 state = putCode code' (putVStack vStack' state)
  where
    code'
      | n == 2 = c1 ++ code
      | n == 1 = c2 ++ code
      | otherwise = error "Condition is not 2 nor 1"
    n : vStack' = getVStack state
    code = getCode state

mkAp :: GmState -> GmState
mkAp state = putHeap heap' (putStack (a : as) state)
  where
    (heap', a) = hAlloc (getHeap state) (NAp a1 a2)
    a1 : a2 : as = getStack state

mkInt :: GmState -> GmState
mkInt state = putVStack vStack' (putHeap heap' (putStack stack' state))
  where
    stack' = a : getStack state
    (heap', a) = hAlloc (getHeap state) (NNum n)
    n : vStack' = getVStack state

mkBool :: GmState -> GmState
mkBool state = putVStack vStack' (putHeap heap' (putStack stack' state))
  where
    stack' = a : getStack state
    (heap', a) = hAlloc (getHeap state) (NConstr tag [])
    tag : vStack' = getVStack state

getInstruction :: GmState -> GmState
getInstruction state
  = case node of
      NNum n -> putVStack (n : getVStack state') state'
      NConstr t [] -> putVStack (t : getVStack state') state'
      _ -> error "Cannot get integer value"
  where
    node = hLookup (getHeap state) a
    state' = putStack stack' state
    a : stack' = getStack state

dyadicIntOp :: (Int -> Int -> Int) -> GmState -> GmState
dyadicIntOp op state = putVStack (op n0 n1 : ns) state
  where
    n0 : n1 : ns = getVStack state

neg :: GmState -> GmState
neg state = putVStack (negate n : ns) state
  where
    n : ns = getVStack state

dyadicBoolOp :: (Int -> Int -> Bool) -> GmState -> GmState
dyadicBoolOp op = dyadicIntOp ((convert .) . op)
  where
    convert True = 2
    convert False = 1

evalInstruction :: GmState -> GmState
evalInstruction state = putCode [Unwind] (putStack [a] (putDump dump' state))
  where
    dump' = (code, as, getVStack state) : getDump state
    a : as = getStack state
    code = getCode state

returnInstruction :: GmState -> GmState
returnInstruction state
  = putStack stack' (putDump dump' (putVStack vStack' (putCode code' state)))
  where
    stack' = last (getStack state) : as
    (code', as, vStack') : dump' = getDump state

unwind :: GmState -> GmState
#if __CLH_EXERCISE_5__ < 9
unwind state = newState (hLookup heap a)
  where
    stack@(a : as) = getStack state
    heap = getHeap state
    newState (NNum n) =
      case getDump state of
        (c, as', vs') : dump' -> putDump dump' (putCode c (putStack (a : as') (putVStack vs' state)))
        _ -> state
    newState (NAp a1 a2) = putCode [Unwind] (putStack (a1 : stack) state)
    newState (NGlobal n c)
      | length as < n =
        case getDump state of
          (c, as', vs') : dump' -> putDump dump' (putCode c (putStack (last stack : as') (putVStack vs' state)))
          _ -> error "Unwinding with too few arguments"
      | otherwise = putCode c (putStack (rearrange n heap stack) state)
    newState (NInd a') = putCode [Unwind] (putStack (a' : as) state)
    newState (NConstr _ _) =
      case getDump state of
        (c, as', vs') : dump' -> putDump dump' (putCode c (putStack (a : as') (putVStack vs' state)))
        _ -> state
#endif

rearrange :: Int -> GmHeap -> GmStack -> GmStack
rearrange n heap as
  = take n as' ++ drop n as
  where
    as' = map (getArg . hLookup heap) (tail as)

getArg :: Node -> Addr
#if __CLH_EXERCISE_5__ < 9
getArg (NAp a1 a2) = a2
#endif

parInstruction :: GmState -> GmState
#if __CLH_EXERCISE_5__ < 18
parInstruction state = putSparks (a : getSparks state) (putStack stack' state)
  where
    a : stack' = getStack state
#endif

printInstruction :: GmState -> GmState
printInstruction state =
  case node of
    NNum n -> putStack as (putOutput (output ++ show n) state)
    NConstr _ args -> putCode (cCode (length args) ++ code) (putStack (args ++ as) state)
    _ -> error "Result is invalid to be printed"
  where
    output = getOutput state
    code = getCode state
    cCode n = concat (replicate n [Eval, Print])
    node = hLookup (getHeap state) a
    a : as = getStack state

#if __CLH_EXERCISE_5__ >= 3
compile :: CoreProgram -> PgmState
compile program
  = (([], heap, globals, [], []), [initialTask addr])
  where
    addr = aLookup globals "main" (error "main is not defined")
    (heap, globals) = buildInitialHeap program

initialTask :: Addr -> PgmLocalState
initialTask addr = (initialCode, [addr], [], [], 0)

initialCode :: GmCode
initialCode = [Eval, Print]

buildInitialHeap :: CoreProgram -> (GmHeap, GmGlobals)
buildInitialHeap program = mapAccumL allocateSc hInitial compiled
  where
    compiled = map compileSc (preludeDefs ++ program ++ primitives)

primitives :: [CoreScDefn]
primitives
  = [ ("+", ["x", "y"], EAp (EAp (EVar "+") (EVar "x")) (EVar "y"))
    , ("-", ["x", "y"], EAp (EAp (EVar "-") (EVar "x")) (EVar "y"))
    , ("*", ["x", "y"], EAp (EAp (EVar "*") (EVar "x")) (EVar "y"))
    , ("/", ["x", "y"], EAp (EAp (EVar "/") (EVar "x")) (EVar "y"))
    , ("negate", ["x"], EAp (EVar "negate") (EVar "x"))
    , ("==", ["x", "y"], EAp (EAp (EVar "==") (EVar "x")) (EVar "y"))
    , ("~=", ["x", "y"], EAp (EAp (EVar "~=") (EVar "x")) (EVar "y"))
    , (">=", ["x", "y"], EAp (EAp (EVar ">=") (EVar "x")) (EVar "y"))
    , (">", ["x", "y"], EAp (EAp (EVar ">") (EVar "x")) (EVar "y"))
    , ("<=", ["x", "y"], EAp (EAp (EVar "<=") (EVar "x")) (EVar "y"))
    , ("<", ["x", "y"], EAp (EAp (EVar "<") (EVar "x")) (EVar "y"))
    , ("if", ["c", "t", "f"], EAp (EAp (EAp (EVar "if") (EVar "c")) (EVar "t")) (EVar "f"))
    , ("True", [], EConstr 2 0)
    , ("False", [], EConstr 1 0)
    , ("par", ["x", "y"], EAp (EAp (EVar "par") (EVar "x")) (EVar "y"))
    ]

type GmCompiledSc = (Name, Int, GmCode)

allocateSc :: GmHeap -> GmCompiledSc -> (GmHeap, (Name, Addr))
allocateSc heap (name, n, is) = (heap', (name, a))
  where
    (heap', a) = hAlloc heap (NGlobal n is)

compileSc :: CoreScDefn -> GmCompiledSc
compileSc (name, argNames, body) = (name, length argNames, compileR body (zip argNames [0..]))

type GmCompiler = CoreExpr -> GmEnvironment -> GmCode
type GmEnvironment = Assoc Name Int

compileR :: GmCompiler
compileR (ELet isRec defs e) env
  | isRec = compileLetRec compileR defs e env
  | otherwise = compileLet compileR defs e env
compileR (EAp (EAp (EAp (EVar "if") e1) e2) e3) env
  = compileB e1 env ++ [Cond (compileR e2 env) (compileR e3 env)]
compileR (EAp (EAp (EVar "par") e1) e2) env
  = compileC e2 env ++ [Push 0, Par]
  ++ compileC e1 (argOffset 1 env)
  ++ [MkAp, Update envLength, Pop envLength, Unwind]
  where
    envLength = length env
compileR (ECase e alters) env
  = compileE e env ++ [CaseJump (compileAlters compileR' alters env)]
compileR (ENum n) env = [PushBasic n, UpdateInt (length env), Return]
compileR (EConstr t 0) env = [PushBasic t, UpdateBool (length env), Return]
compileR e@(EConstr _ _) env = compileE e env ++ [Update (length env), Return]
compileR e env = compileE e env ++ [Update d, Pop d, Unwind]
  where
    d = length env

compileR' :: Int -> GmCompiler
compileR' offset expr env
  = Split offset : compileR expr env

compileE :: GmCompiler
compileE (ENum n) env = [PushInt n]
compileE (ELet isRec defs e) env
  | isRec = compileLetRec compileE defs e env ++ [Slide (length defs)]
  | otherwise = compileLet compileE defs e env ++ [Slide (length defs)]
compileE (ECase e alters) env
  = compileE e env ++ [CaseJump (compileAlters compileE' alters env)]
compileE e@(EAp (EAp (EVar name) _) _) env
  | name `elem` aDomain builtInDyadicInt = compileB e env ++ [MkInt]
  | name `elem` aDomain builtInDyadicBool = compileB e env ++ [MkBool]
compileE e@(EAp (EVar "negate") _) env = compileB e env ++ [MkInt]
compileE (EAp (EAp (EAp (EVar "if") e1) e2) e3) env
  = compileB e1 env ++ [Cond (compileE e2 env) (compileE e3 env)]
compileE (EAp (EAp (EVar "par") e1) e2) env
  = compileC e2 env ++ [Push 0, Par]
  ++ compileC e1 (argOffset 1 env)
  ++ [MkAp, Eval]
compileE e env = compileC e env ++ [Eval]

compileE' :: Int -> GmCompiler
compileE' offset expr env
  = [Split offset] ++ compileE expr env ++ [Slide offset]

compileB :: GmCompiler
compileB (ENum n) env = [PushBasic n]
compileB (ELet isRec defs e) env
  | isRec = compileLetRec compileB defs e env ++ [Pop (length defs)]
  | otherwise = compileLet compileB defs e env ++ [Pop (length defs)]
compileB (EAp (EAp (EVar name) e1) e2) env
  | name `elem` aDomain builtInDyadic = compileB e2 env ++ compileB e1 env ++ [dyadic]
  where
    dyadic = aLookup builtInDyadic name (error "Invalid dyadic operator")
compileB (EAp (EVar "negate") e) env = compileB e env ++ [Neg]
compileB (EAp (EAp (EAp (EVar "negate") e1) e2) e3) env = compileB e1 env ++ [Cond (compileB e2 env) (compileB e3 env)]
compileB e env = compileE e env ++ [Get]

compileC :: GmCompiler
compileC (EVar v) env
  | v `elem` aDomain env = [Push vInd]
  | otherwise = [PushGlobal (GlobalLabel v)]
  where
    vInd = aLookup env v (error "Can't happen")
compileC (ENum n) env = [PushInt n]
compileC (EConstr tag arity) env = [PushGlobal (GlobalPack tag arity)]
compileC (EAp e1 e2) env = compileC e2 env ++ compileC e1 (argOffset 1 env) ++ [MkAp]
compileC (ELet isRec defs e) env
  | isRec = compileLetRec compileC defs e env ++ [Slide (length defs)]
  | otherwise = compileLet compileC defs e env ++ [Slide (length defs)]

compileAlters :: (Int -> GmCompiler) -> [CoreAlter] -> GmEnvironment -> Assoc Int GmCode
compileAlters comp alters env
  = [ (tag, comp (length names) body (zip names [0..] ++ argOffset (length names) env))
    | (tag, names, body) <- alters
    ]

compileLet :: GmCompiler -> [(Name, CoreExpr)] -> GmCompiler
compileLet comp defs expr env
  = compileLet' defs env ++ comp expr env'
  where
    env' = compileArgs defs env

compileLet' :: [(Name, CoreExpr)] -> GmEnvironment -> GmCode
compileLet' [] env = []
compileLet' ((name, expr) : defs) env
  = compileC expr env ++ compileLet' defs (argOffset 1 env)

compileLetRec :: GmCompiler -> [(Name, CoreExpr)] -> GmCompiler
compileLetRec comp defs expr env
  = [Alloc n] ++ compileLetRec' defs env' ++ comp expr env'
  where
    env' = compileArgs defs env
    n = length defs

compileLetRec' :: [(Name, CoreExpr)] -> GmEnvironment -> GmCode
compileLetRec' [] env = []
compileLetRec' ((name, expr) : defs) env
  = compileC expr env ++ [Update (length defs)] ++ compileLetRec' defs env

compileArgs :: [(Name, CoreExpr)] -> GmEnvironment -> GmEnvironment
compileArgs defs env = zip (map fst defs) [n - 1, n - 2 .. 0] ++ argOffset n env
  where
    n = length defs

argOffset :: Int -> GmEnvironment -> GmEnvironment
argOffset n env = [ (v, m + n) | (v, m) <- env ]

builtInDyadic :: Assoc Name Instruction
builtInDyadic = builtInDyadicInt ++ builtInDyadicBool

builtInDyadicInt :: Assoc Name Instruction
builtInDyadicInt = [ ("+", Add), ("-", Sub), ("*", Mul), ("/", Div) ]

builtInDyadicBool :: Assoc Name Instruction
builtInDyadicBool = [ ("==", Eq), ("~=", Ne), ("<", Lt), ("<=", Le), (">", Gt), (">=", Ge) ]

#if __CLH_EXERCISE_5__ >= 5
showResults :: [PgmState] -> String
showSc :: PgmState -> (Name, Addr) -> ISeq
showState :: PgmState -> ISeq
showStats :: PgmState -> ISeq
showOutput :: PgmState -> ISeq
showSparks :: PgmState -> ISeq

showResults states
  = iDisplay resultSeq
  where
    resultSeq
      = iConcat [ iStr "Supercombinator definitions", iNewline
                , iInterleave iNewline (map (showSc state) (pgmGetGlobals state)), iNewline
                , iNewline
                , iStr "State transitions", iNewline
                , iNewline
                , iLayn (map showState states), iNewline
                , iNewline
                , showStats (last states)
                ]

    state = head states

showSc state (name, addr)
  = iConcat [ iStr "Code for ", iStr name, iNewline
            , showInstructions code, iNewline
            ]
    where
      (NGlobal _ code) = hLookup (pgmGetHeap state) addr

showState state
  = iConcat [ showOutput state, iNewline
            , showSparks state, iNewline
            , showTasks state, iNewline
            ]

showOutput state
  = iConcat [ iStr "  Output: \"", iStr (pgmGetOutput state), iStr "\"" ]

#if __CLH_EXERCISE_5__ < 18
showSparks state
  = iConcat [ iStr "  Sparks: [ ", iInterleave (iStr ", ") (map (iStr . showAddr) (pgmGetSparks state)), iStr " ]" ]
#endif

showTasks :: PgmState -> ISeq
showTasks (global, locals)
  = iConcat [ iStr "  Tasks:  { ", iIndent (iInterleave iNewline (map showTask tasks)), iStr " }" ]
  where
    tasks = map ((,) global) locals

showTask :: GmState -> ISeq
showTask task
  = iConcat [ iStr "< "
            , iIndent
              ( iConcat [ showStack task, iNewline
                        , showDump task, iNewline
                        , showVStack task, iNewline
                        , showInstructions (getCode task)
                        ]
              )
            , iStr " >"
            ]

showStack :: GmState -> ISeq
showStack state
  = iConcat [ iStr "  Stack:  [ ", iIndent (iInterleave iNewline (map (showStackItem state) (reverse (getStack state)))), iStr " ]"
            ]

showStackItem :: GmState -> Addr -> ISeq
showStackItem state addr
  = iConcat [ iStr (showAddr addr), iStr ": ", showNode state addr (hLookup (getHeap state) addr)
            ]

#if __CLH_EXERCISE_5__ < 8
showNode :: GmState -> Addr -> Node -> ISeq
showNode state addr (NNum n) = iNum n
showNode state addr (NAp a1 a2)
  = iConcat [ iStr "Ap ", iStr (showAddr a1), iStr " ", iStr (showAddr a2) ]
showNode state addr (NGlobal _ _) = iStr "Global " `iAppend` iStr gName
  where
    gName = head [ name | (name, addr') <- getGlobals state, addr == addr' ]
showNode state addr (NInd a)
  = iConcat [ iStr "Ind ", iStr (showAddr a) ]
showNode state addr (NConstr t as)
  = iConcat [ iStr "Constr ", iNum t, iStr " [", iInterleave (iStr ", ") (map (iStr . showAddr) as), iStr "]" ]
#endif

showDump :: GmState -> ISeq
showDump state
  = iConcat [ iStr "  Dump:   [ "
            , iIndent . iInterleave iNewline . map showDumpItem . reverse . getDump $ state
            , iStr " ]"
            ]

showDumpItem :: GmDumpItem -> ISeq
showDumpItem (code, stack, vStack)
  = iConcat [ iStr "<", shortShowInstructions 3 code
            , iStr ", ", shortShowStack stack
            , iStr ", ", shortShowVStack vStack, iStr ">"
            ]

shortShowInstructions :: Int -> GmCode -> ISeq
shortShowInstructions num code
  = iConcat [ iStr "{", iInterleave (iStr "; ") codeSeqWithDot, iStr "}" ]
  where
    codeSeq = map showInstruction (take num code)
    codeSeqWithDot
      | length code > num = codeSeq ++ [ iStr "..." ]
      | otherwise = codeSeq

shortShowStack :: GmStack -> ISeq
shortShowStack stack
  = iConcat [ iStr "[", iInterleave (iStr ", ") . map (iStr . showAddr) $ stack, iStr "]" ]

shortShowVStack :: GmVStack -> ISeq
shortShowVStack vStack
  = iConcat [ iStr "[", iInterleave (iStr ", ") . map iNum $ vStack, iStr "]" ]

showVStack :: GmState -> ISeq
showVStack state
  = iConcat [ iStr "  VStack: [ ", iInterleave (iStr ",") (map iNum (getVStack state)), iStr " ]" ]

showInstructions :: GmCode -> ISeq
showInstructions is
  = iConcat [ iStr "  Code:   { ", iIndent (iInterleave iNewline (map showInstruction is)), iStr " }" ]

showInstruction :: Instruction -> ISeq
showInstruction (Push n) = iStr "Push " `iAppend` iNum n
showInstruction (PushGlobal f) = iStr "PushGlobal " `iAppend` showGlobalMode f
showInstruction (PushInt n) = iStr "PushInt " `iAppend` iNum n
showInstruction (PushBasic n) = iStr "PushBasic " `iAppend` iNum n
showInstruction (Pop n) = iStr "Pop " `iAppend` iNum n
showInstruction (Slide n) = iStr "Slide " `iAppend` iNum n
showInstruction (Update n) = iStr "Update " `iAppend` iNum n
showInstruction (UpdateInt n) = iStr "UpdateInt " `iAppend` iNum n
showInstruction (UpdateBool n) = iStr "UpdateBool " `iAppend` iNum n
showInstruction (Alloc n) = iStr "Alloc " `iAppend` iNum n
showInstruction (Pack tag arity)
  = iConcat [ iStr "Pack{", iNum tag, iStr ",", iNum arity, iStr "}" ]
showInstruction (Split arity) = iStr "Split " `iAppend` iNum arity
showInstruction (CaseJump alters) = iStr "CaseJump " `iAppend` showAlters alters
showInstruction (Cond c1 c2) = iStr "Cond " `iAppend` showAlters [(2, c1), (1, c2)]
showInstruction MkAp = iStr "MkAp"
showInstruction MkInt = iStr "MkInt"
showInstruction MkBool = iStr "MkBool"
showInstruction Get = iStr "Get"
showInstruction Add = iStr "Add"
showInstruction Sub = iStr "Sub"
showInstruction Mul = iStr "Mul"
showInstruction Div = iStr "Div"
showInstruction Neg = iStr "Neg"
showInstruction Eq = iStr "Eq"
showInstruction Ne = iStr "Ne"
showInstruction Lt = iStr "Lt"
showInstruction Le = iStr "Le"
showInstruction Gt = iStr "Gt"
showInstruction Ge = iStr "Ge"
showInstruction Eval = iStr "Eval"
showInstruction Return = iStr "Return"
showInstruction Unwind = iStr "Unwind"
showInstruction Par = iStr "Par"
showInstruction Print = iStr "Print"

showGlobalMode :: GlobalMode -> ISeq
showGlobalMode (GlobalLabel name) = iStr name
showGlobalMode (GlobalPack tag arity)
  = iConcat [ iStr "Pack{", iNum tag, iStr ",", iNum arity, iStr "}" ]

showAlters :: Assoc Int GmCode -> ISeq
showAlters alters
  = iConcat [ iStr "[ ", iIndent (iInterleave iNewline (map showAlter alters)), iStr " ]"]

showAlter :: (Int, GmCode) -> ISeq
showAlter (tag, code)
  = iConcat [ iNum tag, iStr " -> [ ", iIndent (iInterleave iNewline (map showInstruction code)), iStr " ]" ]

showStats state
  = iConcat [ iStr "Clocks taken = ", iInterleave (iStr ", ") (map iNum (pgmGetStats state)) ]

#if __CLH_EXERCISE_5__ >= 8
#if __CLH_EXERCISE_5__ < 17
data Node
  = NNum Int
  | NAp Addr Addr
  | NGlobal Int GmCode
  | NInd Addr
  | NConstr Int [Addr]
  | NLAp Addr Addr
  | NLGlobal Int GmCode
  deriving (Show, Read, Eq)
#endif

showNode :: GmState -> Addr -> Node -> ISeq
#if __CLH_EXERCISE_5__ < 17
showNode state addr (NNum n) = iNum n
showNode state addr (NAp a1 a2)
  = iConcat [ iStr "Ap ", iStr (showAddr a1), iStr " ", iStr (showAddr a2) ]
showNode state addr (NGlobal _ _) = iStr "Global " `iAppend` iStr gName
  where
    gName = head [ name | (name, addr') <- getGlobals state, addr == addr' ]
showNode state addr (NInd a)
  = iConcat [ iStr "Ind ", iStr (showAddr a) ]
showNode state addr (NConstr t as)
  = iConcat [ iStr "Constr ", iNum t, iStr " [", iInterleave (iStr ", ") (map (iStr . showAddr) as), iStr "]" ]
showNode state addr (NLAp a1 a2)
  = iConcat [ iStr "*Ap ", iStr (showAddr a1), iStr " ", iStr (showAddr a2) ]
showNode state addr (NLGlobal _ _) = iStr "*Global " `iAppend` iStr gName
  where
    gName = head [ name | (name, addr') <- getGlobals state, addr == addr' ]
#endif

#if __CLH_EXERCISE_5__ >= 9
lock :: Addr -> GmState -> GmState
#if __CLH_EXERCISE_5__ < 20
#if __CLH_EXERCISE_5__ < 17
lock addr state
  = putHeap (newHeap (hLookup heap addr)) state
  where
    heap = getHeap state

    newHeap (NAp a1 a2) = hUpdate heap addr (NLAp a1 a2)
    newHeap (NGlobal n c)
      | n == 0 = hUpdate heap addr (NLGlobal n c)
      | otherwise = heap
#else
lock = undefined
#endif
#endif

unlock :: Addr -> GmState -> GmState
#if __CLH_EXERCISE_5__ < 19
#if __CLH_EXERCISE_5__ < 17
unlock addr state
  = newState (hLookup heap addr)
  where
    heap = getHeap state
    newState (NLAp a1 a2)
      = unlock a1 (putHeap (hUpdate heap addr (NAp a1 a2)) state)
    newState (NLGlobal n c)
      = putHeap (hUpdate heap addr (NGlobal n c)) state
    newState _ = state
#else
unlock = undefined
#endif
#endif

update n state = putStack stack' (putHeap heap' unlockedState)
  where
    heap' = hUpdate (getHeap unlockedState) rA (NInd a)
    unlockedState = unlock rA state
    rA = stack' !! n
    a : stack' = getStack state

#if __CLH_EXERCISE_5__ < 21
#if __CLH_EXERCISE_5__ < 17
unwind state = newState (hLookup heap a)
  where
    stack@(a : as) = getStack state
    lockedState = lock a state
    heap = getHeap state
    newState (NNum n) =
      case getDump state of
        (c, as', vs') : dump' -> putDump dump' (putCode c (putStack (a : as') (putVStack vs' state)))
        _ -> state
    newState (NAp a1 a2) = putCode [Unwind] (putStack (a1 : stack) lockedState)
    newState (NGlobal n c)
      | length as < n =
        case getDump state of
          (c, as', vs') : dump' -> putDump dump' (putCode c (putStack (last stack : as') (putVStack vs' state)))
          _ -> error "Unwinding with too few arguments"
      | otherwise = putCode c (putStack (rearrange n heap stack) lockedState)
    newState (NInd a') = putCode [Unwind] (putStack (a' : as) state)
    newState (NConstr _ _) =
      case getDump state of
        (c, as', vs') : dump' -> putDump dump' (putCode c (putStack (a : as') (putVStack vs' state)))
        _ -> state
    newState (NLAp _ _) = putCode [Unwind] state
    newState (NLGlobal _ _) = putCode [Unwind] state
#else
unwind = undefined
#endif
#endif

#if __CLH_EXERCISE_5__ < 21
#if __CLH_EXERCISE_5__ < 17
getArg (NAp a1 a2) = a2
getArg (NLAp a1 a2) = a2
#else
getArg = undefined
#endif
#endif

#if __CLH_EXERCISE_5__ >= 13
machineSize :: Int
machineSize = 4

#if __CLH_EXERCISE_5__ < 14
steps state
  = scheduler global' locals'
  where
    ((output, heap, globals, sparks, stats), locals) = state
    newTasks = [ makeTask s | s <- sparks ]
    global' = (output, heap, globals, [], stats)
    locals' = locals ++ newTasks
#endif

scheduler :: PgmGlobalState -> [PgmLocalState] -> PgmState
#if __CLH_EXERCISE_5__ < 15
#if __CLH_EXERCISE_5__ == 13
scheduler global tasks
  = (global', tasks' ++ nonRunning)
  where
    running = map tick (take machineSize tasks)
    nonRunning = drop machineSize tasks
    (global', tasks') = mapAccumL step global running
#else
scheduler global tasks
  = (global', nonRunning ++ tasks')
  where
    running = map tick (take machineSize tasks)
    nonRunning = drop machineSize tasks
    (global', tasks') = mapAccumL step global running
#endif
#endif

#if __CLH_EXERCISE_5__ >= 14
#if __CLH_EXERCISE_5__ < 18
steps state
  = scheduler global' locals'
  where
    ((output, heap, globals, sparks, stats), locals) = state
    newTasks = map makeTask . take (machineSize - length locals) $ sparks
    global' = (output, heap, globals, [], stats)
    locals' = locals ++ newTasks
#endif

#if __CLH_EXERCISE_5__ >= 15
#if __CLH_EXERCISE_5__ < 22
scheduler global tasks
  = (global', nonRunning ++ tasks')
  where
    (global', tasks') = mapAccumL step global (map tick running)
    (running, nonRunning) = spanN machineSize (isProceedableTask global) tasks
#endif

spanN :: Int -> (a -> Bool) -> [a] -> ([a], [a])
spanN n f l = go n l
  where
    go _ [] = ([], [])
    go 0 as = ([], as)
    go m (a : as)
      = if f a
        then first (a :) (go (m - 1) as)
        else second (a :) (go m as)

isProceedableTask :: PgmGlobalState -> PgmLocalState -> Bool
isProceedableTask global task
  = case code of
      [Eval] -> isReducibleNode state node
      [Unwind] -> not (isLockedNode state node)
      _ -> True
  where
    code = getCode state
    node = hLookup (getHeap state) addr
    addr = head (getStack state)
    state = (global, task)

isReducibleNode :: GmState -> Node -> Bool
#if __CLH_EXERCISE_5__ < 17
isReducibleNode state node
  = case node of
      NNum _ -> False
      NAp _ _ -> True
      NGlobal _ _ -> True
      NInd _ -> True
      NConstr _ _ -> False
      NLAp _ _ -> True
      NLGlobal _ _ -> True
#endif

isLockedNode :: GmState -> Node -> Bool
#if __CLH_EXERCISE_5__ < 17
isLockedNode state node
  = case node of
      NNum _ -> False
      NAp _ _ -> False
      NGlobal _ _ -> False
      NInd _ -> False
      NConstr _ _ -> False
      NLAp _ _ -> True
      NLGlobal _ _ -> True
#endif

#if __CLH_EXERCISE_5__ >= 17
data Node
  = NNum Int
  | NAp Addr Addr
  | NGlobal Int GmCode
  | NInd Addr
  | NConstr Int [Addr]
  | NLAp Addr Addr PgmPendingList
  | NLGlobal Int GmCode PgmPendingList
  deriving (Show, Read, Eq)

type PgmPendingList = [PgmLocalState]

showNode state addr (NNum n) = iNum n
showNode state addr (NAp a1 a2)
  = iConcat [ iStr "Ap ", iStr (showAddr a1), iStr " ", iStr (showAddr a2) ]
showNode state addr (NGlobal _ _) = iStr "Global " `iAppend` iStr gName
  where
    gName = head [ name | (name, addr') <- getGlobals state, addr == addr' ]
showNode state addr (NInd a)
  = iConcat [ iStr "Ind ", iStr (showAddr a) ]
showNode state addr (NConstr t as)
  = iConcat [ iStr "Constr ", iNum t, iStr " [", iInterleave (iStr ", ") (map (iStr . showAddr) as), iStr "]" ]
showNode state addr (NLAp a1 a2 pl)
  = iConcat [ iStr "*Ap ", iStr (showAddr a1), iStr " ", iStr (showAddr a2), iStr " // ", iNum (length pl) ]
showNode state addr (NLGlobal _ _ pl)
  = iConcat [ iStr "*Global ", iStr gName, iStr " // ", iNum (length pl) ]
  where
    gName = head [ name | (name, addr') <- getGlobals state, addr == addr' ]

isReducibleNode state node
  = case node of
      NNum _ -> False
      NAp _ _ -> True
      NGlobal _ _ -> True
      NInd _ -> True
      NConstr _ _ -> False
      NLAp _ _ _ -> True
      NLGlobal _ _ _ -> True

isLockedNode state node
  = case node of
      NNum _ -> False
      NAp _ _ -> False
      NGlobal _ _ -> False
      NInd _ -> False
      NConstr _ _ -> False
      NLAp _ _ _ -> True
      NLGlobal _ _ _ -> True

#if __CLH_EXERCISE_5__ >= 18
type GmSparks = [PgmLocalState]

parInstruction state = putSparks (makeTask a : getSparks state) (putStack stack' state)
  where
    a : stack' = getStack state

showSparks state
  = iConcat [ iStr "  Sparks:  ", iNum . length . pgmGetSparks $ state, iStr " sparks" ]

steps state
  = scheduler global' locals'
  where
    ((output, heap, globals, sparks, stats), locals) = state
    newTasks = take (machineSize - length locals) $ sparks
    global' = (output, heap, globals, [], stats)
    locals' = locals ++ newTasks

#if __CLH_EXERCISE_5__ >= 19
emptyPendingList :: [PgmLocalState] -> GmState -> GmState
emptyPendingList tasks state
  = putSparks (tasks ++ getSparks state) state

unlock addr state
  = newState (hLookup heap addr)
  where
    heap = getHeap state
    newState (NLAp a1 a2 pl)
      = unlock a1 (putHeap (hUpdate heap addr (NAp a1 a2)) (emptyPendingList pl state))
    newState (NLGlobal n c pl)
      = putHeap (hUpdate heap addr (NGlobal n c)) (emptyPendingList pl state)
    newState _ = state

#if __CLH_EXERCISE_5__ >= 20
lock addr state
  = putHeap (newHeap (hLookup heap addr)) state
  where
    heap = getHeap state

    newHeap (NAp a1 a2) = hUpdate heap addr (NLAp a1 a2 [])
    newHeap (NGlobal n c)
      | n == 0 = hUpdate heap addr (NLGlobal n c [])
      | otherwise = heap

#if __CLH_EXERCISE_5__ >= 21
emptyTask :: PgmLocalState
emptyTask = ([], [], [], [], 0)

unwind state = newState (hLookup heap a)
  where
    stack@(a : as) = getStack state
    heap = getHeap state
    (global, _) = state

    lockedState = lock a state
    emptyState = (global, emptyTask)

    newState (NNum n) =
      case getDump state of
        (c, as', vs') : dump' -> putDump dump' (putCode c (putStack (a : as') (putVStack vs' state)))
        _ -> state
    newState (NAp a1 a2) = putCode [Unwind] (putStack (a1 : stack) lockedState)
    newState (NGlobal n c)
      | length as < n =
        case getDump state of
          (c, as', vs') : dump' -> putDump dump' (putCode c (putStack (last stack : as') (putVStack vs' state)))
          _ -> error "Unwinding with too few arguments"
      | otherwise = putCode c (putStack (rearrange n heap stack) lockedState)
    newState (NInd a') = putCode [Unwind] (putStack (a' : as) state)
    newState (NConstr _ _) =
      case getDump state of
        (c, as', vs') : dump' -> putDump dump' (putCode c (putStack (a : as') (putVStack vs' state)))
        _ -> state
    newState (NLAp a1 a2 pl) = putHeap heap' emptyState
      where
        heap' = hUpdate heap a (NLAp a1 a2 (task : pl))
        (_, task) = putCode [Unwind] state
    newState (NLGlobal 0 c pl) = putHeap heap' emptyState
      where
        heap' = hUpdate heap a (NLGlobal 0 c (task : pl))
        (_, task) = putCode [Unwind] state
    newState (NLGlobal n c pl) = putCode [Unwind] state

getArg (NAp _ a2) = a2
getArg (NLAp _ a2 _) = a2

#if __CLH_EXERCISE_5__ >= 22
scheduler global@(output, heap, globals, _, stats) tasks
  = mapAccumL step (output, heap, globals, nonRunning, stats) (map tick running)
  where
    (running, nonRunning) = spanN machineSize (isProceedableTask global) tasks

#if __CLH_EXERCISE_5__ >= 23
doAdmin ((output, heap, globals, sparks, stats), locals)
  = ((output, heap, globals, sparks, stats'), locals')
  where
    (locals', stats') = foldr makeNewLocalAndStats ([], stats) locals

    makeNewLocalAndStats local@(code, _, _, _, clock) (localsAcc, statsAcc)
      | not (null code) = (local : localsAcc, statsAcc)
      | clock > 0 = (localsAcc, clock : statsAcc)
      | otherwise = (localsAcc, statsAcc)
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
#endif
#endif
#endif
