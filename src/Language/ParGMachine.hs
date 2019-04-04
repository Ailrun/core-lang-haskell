{-# LANGUAGE CPP #-}
module Language.ParGMachine
  ( parGMRun
  , run
  , showResults
  , eval
  , compile
  )
where

import Data.List
import Data.ISeq
import Language.Parser
import Language.Prelude
import Language.Types
import Util

parGMRun = putStrLn . run

run :: String -> String
run = showResults . eval . compile . parse

getOutput :: GmState -> GmOutput
putOutput :: GmOutput -> GmState -> GmState

getCode :: GmState -> GmCode
putCode :: GmCode -> GmState -> GmState

getStack :: GmState -> GmStack
putStack :: GmStack -> GmState -> GmState

getDump :: GmState -> GmDump
putDump :: GmDump -> GmState -> GmState

getVStack :: GmState -> GmVStack
putVStack :: GmVStack -> GmState -> GmState

getHeap :: GmState -> GmHeap
putHeap :: GmHeap -> GmState -> GmState

getGlobals :: GmState -> GmGlobals
putGlobals :: GmGlobals -> GmState -> GmState

getStats :: GmState -> GmStats
putStats :: GmStats -> GmState -> GmState

type GmState = (GmOutput, GmCode, GmStack, GmDump, GmVStack, GmHeap, GmGlobals, GmStats)

getOutput (output, _, _, _, _, _, _, _) = output
putOutput output (_, code, stack, dump, vStack, heap, globals, stats) = (output, code, stack, dump, vStack, heap, globals, stats)

getCode (_, code, _, _, _, _, _, _) = code
putCode code (output, _, stack, dump, vStack, heap, globals, stats) = (output, code, stack, dump, vStack, heap, globals, stats)

getStack (_, _, stack, _, _, _, _, _) = stack
putStack stack (output, code, _, dump, vStack, heap, globals, stats) = (output, code, stack, dump, vStack, heap, globals, stats)

getDump (_, _, _, dump, _, _, _, _) = dump
putDump dump (output, code, stack, _, vStack, heap, globals, stats) = (output, code, stack, dump, vStack, heap, globals, stats)

getVStack (_, _, _, _, vStack, _, _, _) = vStack
putVStack vStack (output, code, stack, dump, _, heap, globals, stats) = (output, code, stack, dump, vStack, heap, globals, stats)

getHeap (_, _, _, _, _, heap, _, _) = heap
putHeap heap (output, code, stack, dump, vStack, _, globals, stats) = (output, code, stack, dump, vStack, heap, globals, stats)

getGlobals (_, _, _, _, _, _, globals, _) = globals
putGlobals globals (output, code, stack, dump, vStack, heap, _, stats) = (output, code, stack, dump, vStack, heap, globals, stats)

getStats (_, _, _, _, _, _, _, stats) = stats
putStats stats (output, code, stack, dump, vStack, heap, globals, _) = (output, code, stack, dump, vStack, heap, globals, stats)

type GmOutput = String

type GmCode = [Instruction]
data Instruction
  = Unwind
  | PushGlobal GlobalMode
  | PushInt Int
  | Push Int
  | MkAp
  | Update Int
  | Pop Int
  | Alloc Int
  | Slide Int
  | Eval
  | Add | Sub | Mul | Div
  | Neg
  | Eq | Ne | Lt | Le | Gt | Ge
  | Cond GmCode GmCode
  | Pack Int Int
  | CaseJump (Assoc Int GmCode)
  | Split Int
  | Print
  | PushBasic Int
  | MkBool
  | MkInt
  | Get
  | Return
  | UpdateInt Int
  | UpdateBool Int
  deriving (Show, Read, Eq)
data GlobalMode
  = GlobalLabel Name
  | GlobalPack Int Int
  deriving (Show, Read, Eq)

type GmStack = [Addr]

type GmDump = [GmDumpItem]
type GmDumpItem = (GmCode, GmStack, GmVStack)

type GmVStack = [Int]

type GmHeap = Heap Node
data Node
  = NNum Int
  | NAp Addr Addr
  | NGlobal Int GmCode
  | NInd Addr
  | NConstr Int [Addr]
  deriving (Show, Read, Eq)

type GmGlobals = Assoc Name Addr

statInitial :: GmStats
statIncSteps :: GmStats -> GmStats
statGetSteps :: GmStats -> Int

type GmStats = Int

statInitial = 0
statIncSteps s = s + 1
statGetSteps s = s

eval :: GmState -> [GmState]
eval state = state : restStates
  where
    restStates
      | gmFinal state = []
      | otherwise = eval nextState
    nextState = doAdmin (step state)

doAdmin :: GmState -> GmState
doAdmin s = putStats (statIncSteps (getStats s)) s

gmFinal :: GmState -> Bool
gmFinal = null . getCode

step :: GmState -> GmState
step state = dispatch i (putCode is state)
  where
    i : is = getCode state

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
dispatch MkBool = mkBool
dispatch MkInt = mkInt
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
dispatch Unwind = unwind
dispatch Eval = evalInstruction
dispatch Return = returnInstruction
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
update n state = putStack stack' (putHeap heap' state)
  where
    heap' = hUpdate (getHeap state) rA (NInd a)
    rA = stack' !! n
    a : stack' = getStack state

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

mkBool :: GmState -> GmState
mkBool state = putVStack vStack' (putHeap heap' (putStack stack' state))
  where
    stack' = a : getStack state
    (heap', a) = hAlloc (getHeap state) (NConstr tag [])
    tag : vStack' = getVStack state

mkInt :: GmState -> GmState
mkInt state = putVStack vStack' (putHeap heap' (putStack stack' state))
  where
    stack' = a : getStack state
    (heap', a) = hAlloc (getHeap state) (NNum n)
    n : vStack' = getVStack state

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

unwind :: GmState -> GmState
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

rearrange :: Int -> GmHeap -> GmStack -> GmStack
rearrange n heap as
  = take n as' ++ drop n as
  where
    as' = map (getArg . hLookup heap) (tail as)

getArg :: Node -> Addr
getArg (NAp a1 a2) = a2

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

compile :: CoreProgram -> GmState
compile program
  = ([], initialCode, [], [], [], heap, globals, statInitial)
  where
    (heap, globals) = buildInitialHeap program

initialCode :: GmCode
initialCode = [PushGlobal (GlobalLabel "main"), Eval, Print]

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

showResults states
  = iDisplay resultSeq
  where
    resultSeq
      = iConcat [ iStr "Supercombinator definitions", iNewline
                , iInterleave iNewline (map (showSc state) (getGlobals state)), iNewline
                , iNewline
                , iStr "State transitions", iNewline
                , iNewline
                , iLayn (map showState states), iNewline
                , iNewline
                , showStats (last states)
                ]

    state : _ = states

showSc :: GmState -> (Name, Addr) -> ISeq
showSc state (name, addr)
  = iConcat [ iStr "Code for ", iStr name, iNewline
            , showInstructions code, iNewline
            ]
    where
      (NGlobal _ code) = hLookup (getHeap state) addr

showState :: GmState -> ISeq
showState state
  = iConcat [ showOutput state, iNewline
            , showStack state, iNewline
            , showDump state, iNewline
            , showVStack state, iNewline
            , showInstructions (getCode state), iNewline
            ]

showOutput :: GmState -> ISeq
showOutput state = iConcat [ iStr "  Output: \"", iStr (getOutput state), iStr "\"" ]

showStack :: GmState -> ISeq
showStack state
  = iConcat [ iStr "  Stack:  [ ", iIndent (iInterleave iNewline (map (showStackItem state) (reverse (getStack state)))), iStr " ]"
            ]

showStackItem :: GmState -> Addr -> ISeq
showStackItem state addr
  = iConcat [ iStr (showAddr addr), iStr ": ", showNode state addr (hLookup (getHeap state) addr)
            ]

showNode :: GmState -> Addr -> Node -> ISeq
showNode state addr (NNum n) = iNum n
showNode state addr (NGlobal _ _) = iStr "Global " `iAppend` iStr gName
  where
    gName = head [ name | (name, addr') <- getGlobals state, addr == addr' ]
showNode state addr (NAp a1 a2)
  = iConcat [ iStr "Ap ", iStr (showAddr a1), iStr " ", iStr (showAddr a2) ]
showNode state addr (NInd a)
  = iConcat [ iStr "Ind ", iStr (showAddr a) ]
showNode state addr (NConstr t as)
  = iConcat [ iStr "Constr ", iNum t, iStr " [", iInterleave (iStr ", ") (map (iStr . showAddr) as), iStr "]" ]

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

showStats :: GmState -> ISeq
showStats state
  = iConcat [ iStr "Steps taken = ", iNum (statGetSteps (getStats state)) ]

showInstructions :: GmCode -> ISeq
showInstructions is
  = iConcat [ iStr "  Code:   { ", iIndent (iInterleave iNewline (map showInstruction is)), iStr " }", iNewline
            ]

showInstruction :: Instruction -> ISeq
showInstruction Unwind = iStr "Unwind"
showInstruction (PushGlobal f) = iStr "PushGlobal " `iAppend` showGlobalMode f
showInstruction (Push n) = iStr "Push " `iAppend` iNum n
showInstruction (PushInt n) = iStr "PushInt " `iAppend` iNum n
showInstruction MkAp = iStr "MkAp"
showInstruction (Update n) = iStr "Update " `iAppend` iNum n
showInstruction (UpdateInt n) = iStr "UpdateInt " `iAppend` iNum n
showInstruction (UpdateBool n) = iStr "UpdateBool " `iAppend` iNum n
showInstruction (Pop n) = iStr "Pop " `iAppend` iNum n
showInstruction (Alloc n) = iStr "Alloc " `iAppend` iNum n
showInstruction (Slide n) = iStr "Slide " `iAppend` iNum n
showInstruction Eval = iStr "Eval"
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
showInstruction (Cond c1 c2) = iStr "Cond " `iAppend` showAlters [(2, c1), (1, c2)]
showInstruction (Pack tag arity)
  = iConcat [ iStr "Pack{", iNum tag, iStr ",", iNum arity, iStr "}" ]
showInstruction (CaseJump alters) = iStr "CaseJump " `iAppend` showAlters alters
showInstruction (Split arity) = iStr "Split " `iAppend` iNum arity
showInstruction (PushBasic n) = iStr "PushBasic " `iAppend` iNum n
showInstruction MkBool = iStr "MkBool"
showInstruction MkInt = iStr "MkInt"
showInstruction Get = iStr "Get"
showInstruction Print = iStr "Print"
showInstruction Return = iStr "Return"

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
