{-# LANGUAGE CPP #-}
module Language.GMachine
#if __CLH_EXERCISE_1__ >= 8
  ( run
  , showResults
  , eval
  , compile
  )
#endif
where

#if __CLH_EXERCISE_1__ >= 8
import Data.List
import Data.ISeq
import Language.Parser
import Language.Prelude
import Language.Types
import Text.ParserCombinators.ReadP
import Util

run :: String -> String
run = showResults . eval . compile . parse

#if __CLH_EXERCISE_3__ < 21
type GmState = (GmCode, GmStack, GmHeap, GmGlobals, GmStats)
#endif

type GmCode = [Instruction]

getCode :: GmState -> GmCode
putCode :: GmCode -> GmState -> GmState
#if __CLH_EXERCISE_3__ < 21
getCode (code, _, _, _, _) = code
putCode code (_, stack, heap, globals, stats) = (code, stack, heap, globals, stats)
#endif

#if __CLH_EXERCISE_3__ < 7
data Instruction
  = Unwind
  | PushGlobal Name
  | PushInt Int
  | Push Int
  | MkAp
  | Slide Int
  deriving (Show, Read, Eq)
#endif

type GmStack = [Addr]

getStack :: GmState -> GmStack
putStack :: GmStack -> GmState -> GmState
#if __CLH_EXERCISE_3__ < 21
getStack (_, stack, _, _, _) = stack
putStack stack (code, _, heap, globals, stats) = (code, stack, heap, globals, stats)
#endif

type GmHeap = Heap Node

getHeap :: GmState -> GmHeap
putHeap :: GmHeap -> GmState -> GmState
#if __CLH_EXERCISE_3__ < 21
getHeap (_, _, heap, _, _) = heap
putHeap heap (code, stack, _, globals, stats) = (code, stack, heap, globals, stats)
#endif

#if __CLH_EXERCISE_3__ < 8
data Node
  = NNum Int
  | NAp Addr Addr
  | NGlobal Int GmCode
  deriving (Show, Read, Eq)
#endif

type GmGlobals = Assoc Name Addr

getGlobals :: GmState -> GmGlobals
#if __CLH_EXERCISE_3__ < 21
getGlobals (_, _, _, globals, _) = globals
#endif

statInitial :: GmStats
statIncSteps :: GmStats -> GmStats
statGetSteps :: GmStats -> Int

type GmStats = Int

statInitial = 0
statIncSteps s = s + 1
statGetSteps s = s

getStats :: GmState -> GmStats
putStats :: GmStats -> GmState -> GmState
#if __CLH_EXERCISE_3__ < 21
getStats (_, _, _, _, stats) = stats
putStats stats (code, stack, heap, globals, _) = (code, stack, heap, globals, stats)
#endif

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
#if __CLH_EXERCISE_3__ < 9
#if __CLH_EXERCISE_3__ < 7
dispatch (PushGlobal f) = pushGlobal f
dispatch (PushInt n) = pushInt n
dispatch MkAp = mkAp
dispatch (Push n) = push n
dispatch (Slide n) = slide n
dispatch Unwind = unwind
#else
dispatch = undefined
#endif
#endif

pushGlobal :: Name -> GmState -> GmState
#if __CLH_EXERCISE_3__ < 38
pushGlobal name state = putStack (a : getStack state) state
  where
    a = aLookup (getGlobals state) name (error ("Undeclared global " ++ name))
#endif

pushInt :: Int -> GmState -> GmState
#if __CLH_EXERCISE_3__ < 6
pushInt n state = putHeap heap' (putStack (a : getStack state) state)
  where
    (heap', a) = hAlloc (getHeap state) (NNum n)
#endif

mkAp :: GmState -> GmState
mkAp state = putHeap heap' (putStack (a : as) state)
  where
    (heap', a) = hAlloc (getHeap state) (NAp a1 a2)
    a1 : a2 : as = getStack state

push :: Int -> GmState -> GmState
#if __CLH_EXERCISE_3__ < 12
push n state = putStack (a : as) state
  where
    as = getStack state
    a = getArg (hLookup (getHeap state) (as !! (n + 1)))
#endif

getArg :: Node -> Addr
getArg (NAp a1 a2) = a2

slide :: Int -> GmState -> GmState
slide n state = putStack (a : drop n as) state
  where
    a : as = getStack state

unwind :: GmState -> GmState
#if __CLH_EXERCISE_3__ < 9
unwind state = newState (hLookup heap a)
  where
    stack@(a : as) = getStack state
    heap = getHeap state
    newState (NNum n) = state
    newState (NAp a1 a2) = putCode [Unwind] (putStack (a1 : stack) state)
    newState (NGlobal n c)
      | length as < n = error "Unwinding with too few arguments"
      | otherwise = putCode c state
#endif

compile :: CoreProgram -> GmState
#if __CLH_EXERCISE_3__ < 27
#if __CLH_EXERCISE_3__ < 21
compile program = (initialCode, [], heap, globals, statInitial)
  where
    (heap, globals) = buildInitialHeap program
#else
compile = undefined
#endif
#endif

buildInitialHeap :: CoreProgram -> (GmHeap, GmGlobals)
#if __CLH_EXERCISE_3__ < 41
buildInitialHeap program = mapAccumL allocateSc hInitial compiled
  where
    compiled = map compileSc (preludeDefs ++ program) ++ compiledPrimitives
#endif

type GmCompiledSc = (Name, Int, GmCode)

allocateSc :: GmHeap -> GmCompiledSc -> (GmHeap, (Name, Addr))
allocateSc heap (name, n, is) = (heap', (name, a))
  where
    (heap', a) = hAlloc heap (NGlobal n is)

initialCode :: GmCode
#if __CLH_EXERCISE_3__ < 27
initialCode = [PushGlobal "main", Unwind]
#endif

compileSc :: CoreScDefn -> GmCompiledSc
compileSc (name, argNames, body) = (name, length argNames, compileR body (zip argNames [0..]))

compileR :: GmCompiler
#if __CLH_EXERCISE_3__ < 10
#if __CLH_EXERCISE_3__ < 7
compileR e env = compileC e env ++ [Slide (length env + 1), Unwind]
#else
compileR = undefined
#endif
#endif

type GmCompiler = CoreExpr -> GmEnvironment -> GmCode

type GmEnvironment = Assoc Name Int

compileC :: GmCompiler
#if __CLH_EXERCISE_3__ < 16
compileC (EVar v) env
  | v `elem` aDomain env = [Push vInd]
  | otherwise = [PushGlobal v]
  where
    vInd = aLookup env v (error "Can't happen")
compileC (ENum n) env = [PushInt n]
compileC (EAp e1 e2) env = compileC e2 env ++ compileC e1 (argOffset 1 env) ++ [MkAp]
#endif

argOffset :: Int -> GmEnvironment -> GmEnvironment
argOffset n env = [ (v, m + n) | (v, m) <- env ]

compiledPrimitives :: [GmCompiledSc]
#if __CLH_EXERCISE_3__ < 27
compiledPrimitives = []
#endif

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

showInstructions :: GmCode -> ISeq
showInstructions is
  = iConcat [ iStr "  Code:   { ", iIndent (iInterleave iNewline (map showInstruction is)), iStr " }", iNewline
            ]

showInstruction :: Instruction -> ISeq
#if __CLH_EXERCISE_3__ < 7
showInstruction Unwind = iStr "Unwind"
showInstruction (PushGlobal f) = iStr "PushGlobal " `iAppend` iStr f
showInstruction (Push n) = iStr "Push " `iAppend` iNum n
showInstruction (PushInt n) = iStr "PushInt " `iAppend` iNum n
showInstruction MkAp = iStr "MkAp"
showInstruction (Slide n) = iStr "Slide " `iAppend` iNum n
#endif

showState :: GmState -> ISeq
#if __CLH_EXERCISE_3__ < 22
showState state
  = iConcat [ showStack state, iNewline
            , showInstructions (getCode state), iNewline
            ]
#endif

showStack :: GmState -> ISeq
showStack state
  = iConcat [ iStr "  Stack:  [ ", iIndent (iInterleave iNewline (map (showStackItem state) (reverse (getStack state)))), iStr " ]"
            ]

showStackItem :: GmState -> Addr -> ISeq
showStackItem state addr
  = iConcat [ iStr (showAddr addr), iStr ": ", showNode state addr (hLookup (getHeap state) addr)
            ]

showNode :: GmState -> Addr -> Node -> ISeq
#if __CLH_EXERCISE_3__ < 7
showNode state addr (NNum n) = iNum n
showNode state addr (NGlobal _ _) = iStr "Global " `iAppend` iStr gName
  where
    gName = head [ name | (name, addr') <- getGlobals state, addr == addr' ]
showNode state addr (NAp a1 a2)
  = iConcat [ iStr "Ap ", iStr (showAddr a1), iStr " ", iStr (showAddr a2) ]
#endif

showStats :: GmState -> ISeq
showStats state
  = iConcat [ iStr "Steps taken = ", iNum (statGetSteps (getStats state)) ]

#if __CLH_EXERCISE_3__ >= 6
putGlobals :: GmGlobals -> GmState -> GmState
#if __CLH_EXERCISE_3__ < 21
putGlobals globals (code, stack, heap, _, stats) = (code, stack, heap, globals, stats)
#endif

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

#if __CLH_EXERCISE_3__ >= 7
#if __CLH_EXERCISE_3__ < 14
data Instruction
  = Unwind
  | PushGlobal Name
  | PushInt Int
  | Push Int
  | MkAp
  | Update Int
  | Pop Int
  deriving (Show, Read, Eq)

showInstruction Unwind = iStr "Unwind"
showInstruction (PushGlobal f) = iStr "PushGlobal " `iAppend` iStr f
showInstruction (Push n) = iStr "Push " `iAppend` iNum n
showInstruction (PushInt n) = iStr "PushInt " `iAppend` iNum n
showInstruction MkAp = iStr "MkAp"
showInstruction (Update n) = iStr "Update " `iAppend` iNum n
showInstruction (Pop n) = iStr "Pop " `iAppend` iNum n
#endif

#if __CLH_EXERCISE_3__ >= 8
#if __CLH_EXERCISE_3__ < 31
data Node
  = NNum Int
  | NAp Addr Addr
  | NGlobal Int GmCode
  | NInd Addr
  deriving (Show, Read, Eq)

showNode state addr (NNum n) = iNum n
showNode state addr (NGlobal _ _) = iStr "Global " `iAppend` iStr gName
  where
    gName = head [ name | (name, addr') <- getGlobals state, addr == addr' ]
showNode state addr (NAp a1 a2)
  = iConcat [ iStr "Ap ", iStr (showAddr a1), iStr " ", iStr (showAddr a2) ]
showNode state addr (NInd a)
  = iConcat [ iStr "Ind ", iStr (showAddr a) ]
#endif

#if __CLH_EXERCISE_3__ >= 9
#if __CLH_EXERCISE_3__ < 15
dispatch (PushGlobal f) = pushGlobal f
dispatch (PushInt n) = pushInt n
dispatch MkAp = mkAp
dispatch (Push n) = push n
dispatch (Update n) = update n
dispatch (Pop n) = pop n
dispatch Unwind = unwind
#endif

update :: Int -> GmState -> GmState
update n state = putStack stack' (putHeap heap' state)
  where
    heap' = hUpdate (getHeap state) rA (NInd a)
    rA = stack' !! n
    a : stack' = getStack state

pop :: Int -> GmState -> GmState
pop n state = putStack (drop n (getStack state)) state

#if __CLH_EXERCISE_3__ < 12
unwind state = newState (hLookup heap a)
  where
    stack@(a : as) = getStack state
    heap = getHeap state
    newState (NNum n) = state
    newState (NAp a1 a2) = putCode [Unwind] (putStack (a1 : stack) state)
    newState (NGlobal n c)
      | length as < n = error "Unwinding with too few arguments"
      | otherwise = putCode c state
    newState (NInd a') = putCode [Unwind] (putStack (a' : as) state)
#endif

#if __CLH_EXERCISE_3__ >= 10
#if __CLH_EXERCISE_3__ < 28
compileR e env = compileC e env ++ [Update d, Pop d, Unwind]
  where
    d = length env
#endif

#if __CLH_EXERCISE_3__ >= 12
push n state = putStack (a : as) state
  where
    as = getStack state
    a = as !! n

#if __CLH_EXERCISE_3__ < 23
unwind state = newState (hLookup heap a)
  where
    stack@(a : as) = getStack state
    heap = getHeap state
    newState (NNum n) = state
    newState (NAp a1 a2) = putCode [Unwind] (putStack (a1 : stack) state)
    newState (NGlobal n c)
      | length as < n = error "Unwinding with too few arguments"
      | otherwise = putCode c (putStack (rearrange n heap stack) state)
    newState (NInd a') = putCode [Unwind] (putStack (a' : as) state)
#endif

rearrange :: Int -> GmHeap -> GmStack -> GmStack
rearrange n heap as
  = take n as' ++ drop n as
  where
    as' = map (getArg . hLookup heap) (tail as)

#if __CLH_EXERCISE_3__ >= 14
#if __CLH_EXERCISE_3__ < 22
data Instruction
  = Unwind
  | PushGlobal Name
  | PushInt Int
  | Push Int
  | MkAp
  | Update Int
  | Pop Int
  | Alloc Int
  | Slide Int
  deriving (Show, Read, Eq)

showInstruction Unwind = iStr "Unwind"
showInstruction (PushGlobal f) = iStr "PushGlobal " `iAppend` iStr f
showInstruction (Push n) = iStr "Push " `iAppend` iNum n
showInstruction (PushInt n) = iStr "PushInt " `iAppend` iNum n
showInstruction MkAp = iStr "MkAp"
showInstruction (Update n) = iStr "Update " `iAppend` iNum n
showInstruction (Pop n) = iStr "Pop " `iAppend` iNum n
showInstruction (Alloc n) = iStr "Alloc " `iAppend` iNum n
showInstruction (Slide n) = iStr "Slide " `iAppend` iNum n
#endif

#if __CLH_EXERCISE_3__ >= 15
allocNodes :: Int -> GmHeap -> (GmHeap, [Addr])
allocNodes 0 heap = (heap, [])
allocNodes n heap = (heap'', a : as)
  where
    (heap', as) = allocNodes (n - 1) heap
    (heap'', a) = hAlloc heap' (NInd hNull)

#if __CLH_EXERCISE_3__ < 23
dispatch (PushGlobal f) = pushGlobal f
dispatch (PushInt n) = pushInt n
dispatch MkAp = mkAp
dispatch (Push n) = push n
dispatch (Update n) = update n
dispatch (Pop n) = pop n
dispatch (Alloc n) = alloc n
dispatch (Slide n) = slide n
dispatch Unwind = unwind
#endif

alloc :: Int -> GmState -> GmState
alloc n state = putHeap heap' (putStack stack' state)
  where
    stack' = as ++ (getStack state)
    (heap', as) = allocNodes n (getHeap state)

#if __CLH_EXERCISE_3__ >= 16
#if __CLH_EXERCISE_3__ < 35
compileC (EVar v) env
  | v `elem` aDomain env = [Push vInd]
  | otherwise = [PushGlobal v]
  where
    vInd = aLookup env v (error "Can't happen")
compileC (ENum n) env = [PushInt n]
compileC (EAp e1 e2) env = compileC e2 env ++ compileC e1 (argOffset 1 env) ++ [MkAp]
compileC (ELet isRec defs e) env
  | isRec = compileLetRec compileC defs e env
  | otherwise = compileLet compileC defs e env
#endif

compileLet :: GmCompiler -> [(Name, CoreExpr)] -> GmCompiler
compileLet comp defs expr env
  = compileLet' defs env ++ comp expr env'
  where
    env' = compileArgs defs env

compileLet' :: [(Name, CoreExpr)] -> GmEnvironment -> GmCode
compileLet' [] env = []
compileLet' ((name, expr) : defs) env
  = compileC expr env ++ compileLet' defs (argOffset 1 env)

compileArgs :: [(Name, CoreExpr)] -> GmEnvironment -> GmEnvironment
compileArgs defs env = zip (map fst defs) [n - 1, n - 2 .. 0] ++ argOffset n env
  where
    n = length defs

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

#if __CLH_EXERCISE_3__ >= 21
#if __CLH_EXERCISE_3__ < 31
type GmState = (GmCode, GmStack, GmDump, GmHeap, GmGlobals, GmStats)
#endif

type GmDump = [GmDumpItem]

#if __CLH_EXERCISE_3__ < 44
type GmDumpItem = (GmCode, GmStack)
#endif

getDump :: GmState -> GmDump
putDump :: GmDump -> GmState -> GmState
#if __CLH_EXERCISE_3__ < 31
getDump (_, _, dump, _, _, _) = dump
putDump dump (code, stack, _, heap, globals, stats) = (code, stack, dump, heap, globals, stats)

getCode (code, _, _, _, _, _) = code
putCode code (_, stack, dump, heap, globals, stats) = (code, stack, dump, heap, globals, stats)

getStack (_, stack, _, _, _, _) = stack
putStack stack (code, _, dump, heap, globals, stats) = (code, stack, dump, heap, globals, stats)

getHeap (_, _, _, heap, _, _) = heap
putHeap heap (code, stack, dump, _, globals, stats) = (code, stack, dump, heap, globals, stats)

getGlobals (_, _, _, _, globals, _) = globals
putGlobals globals (code, stack, dump, heap, _, stats) = (code, stack, dump, heap, globals, stats)

getStats (_, _, _, _, _, stats) = stats
putStats stats (code, stack, dump, heap, globals, _) = (code, stack, dump, heap, globals, stats)
#endif

#if __CLH_EXERCISE_3__ >= 22
#if __CLH_EXERCISE_3__ < 32
data Instruction
  = Unwind
  | PushGlobal Name
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
  deriving (Show, Read, Eq)
#endif

#if __CLH_EXERCISE_3__ < 32
showInstruction Unwind = iStr "Unwind"
showInstruction (PushGlobal f) = iStr "PushGlobal " `iAppend` iStr f
showInstruction (Push n) = iStr "Push " `iAppend` iNum n
showInstruction (PushInt n) = iStr "PushInt " `iAppend` iNum n
showInstruction MkAp = iStr "MkAp"
showInstruction (Update n) = iStr "Update " `iAppend` iNum n
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
showInstruction (Cond c1 c2)
  = iConcat [ iStr "Cond [ ", iIndent showCases, iStr " ]" ]
  where
    showCases
      = iConcat [ iStr "True  -> [ ", showCase c1, iStr " ]", iNewline,
                  iStr "False -> [ ", showCase c2, iStr " ]"]
    showCase c = iIndent (iInterleave iNewline (map showInstruction c))
#endif

#if __CLH_EXERCISE_3__ < 31
showState state
  = iConcat [ showStack state, iNewline
            , showDump state, iNewline
            , showInstructions (getCode state), iNewline
            ]
#endif

showDump :: GmState -> ISeq
showDump state
  = iConcat [ iStr "  Dump:   [ "
            , iIndent . iInterleave iNewline . map showDumpItem . reverse . getDump $ state
            , iStr " ]"
            ]

showDumpItem :: GmDumpItem -> ISeq
#if __CLH_EXERCISE_3__ < 44
showDumpItem (code, stack)
  = iConcat [ iStr "<", shortShowInstructions 3 code, iStr ", ", shortShowStack stack, iStr ">" ]
#endif

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

#if __CLH_EXERCISE_3__ >= 23
type BoxingF a = a -> GmState -> GmState
type UnboxingF a = Addr -> GmState -> a

boxInteger :: BoxingF Int
boxInteger n state
  = putStack (a : getStack state) (putHeap heap' state)
  where
    (heap', a) = hAlloc (getHeap state) (NNum n)

unboxInteger :: UnboxingF Int
unboxInteger a state
  = unbox (hLookup (getHeap state) a)
  where
    unbox (NNum i) = i
    unbox _ = error "Unboxing a non-integer"

primitive1 :: BoxingF b -> UnboxingF a -> (a -> b) -> GmState -> GmState
primitive1 box unbox op state
  = box (op (unbox a state)) (putStack as state)
  where
    a : as = getStack state

primitive2 :: BoxingF b -> UnboxingF a -> (a -> a -> b) -> GmState -> GmState
primitive2 box unbox op state
  = box (op (unbox a0 state) (unbox a1 state)) (putStack as state)
  where
    a0 : a1 : as = getStack state

arithmetic1 :: (Int -> Int) -> GmState -> GmState
arithmetic1 = primitive1 boxInteger unboxInteger

arithmetic2 :: (Int -> Int -> Int) -> GmState -> GmState
arithmetic2 = primitive2 boxInteger unboxInteger

#if __CLH_EXERCISE_3__ < 25
dispatch (PushGlobal f) = pushGlobal f
dispatch (PushInt n) = pushInt n
dispatch MkAp = mkAp
dispatch (Push n) = push n
dispatch (Update n) = update n
dispatch (Pop n) = pop n
dispatch (Alloc n) = alloc n
dispatch (Slide n) = slide n
dispatch Add = arithmetic2 (+)
dispatch Sub = arithmetic2 (-)
dispatch Mul = arithmetic2 (*)
dispatch Div = arithmetic2 div
dispatch Neg = arithmetic1 negate
dispatch Unwind = unwind
dispatch Eval = evalInstruction
#endif

#if __CLH_EXERCISE_3__ < 29
unwind state = newState (hLookup heap a)
  where
    stack@(a : as) = getStack state
    heap = getHeap state
    newState (NNum n)
      = case getDump state of
        (c, as') : dump' -> putDump dump' (putCode c (putStack (a : as') state))
        _ -> state
    newState (NAp a1 a2) = putCode [Unwind] (putStack (a1 : stack) state)
    newState (NGlobal n c)
      | length as < n = error "Unwinding with too few arguments"
      | otherwise = putCode c (putStack (rearrange n heap stack) state)
    newState (NInd a') = putCode [Unwind] (putStack (a' : as) state)
#endif

evalInstruction :: GmState -> GmState
#if __CLH_EXERCISE_3__ < 44
evalInstruction state = putCode [Unwind] (putStack [a] (putDump dump' state))
  where
    dump' = (code, as) : getDump state
    a : as = getStack state
    code = getCode state
#endif

#if __CLH_EXERCISE_3__ >= 25
boxBoolean :: Bool -> GmState -> GmState
#if __CLH_EXERCISE_3__ < 37
boxBoolean b state
  = putStack (a : getStack state) (putHeap heap' state)
  where
    (heap', a) = hAlloc (getHeap state) (NNum b')
    b' | b = 1
       | otherwise = 0
#endif

comparison :: (Int -> Int -> Bool) -> GmState -> GmState
comparison = primitive2 boxBoolean unboxInteger

#if __CLH_EXERCISE_3__ < 33
dispatch (PushGlobal f) = pushGlobal f
dispatch (PushInt n) = pushInt n
dispatch MkAp = mkAp
dispatch (Push n) = push n
dispatch (Update n) = update n
dispatch (Pop n) = pop n
dispatch (Alloc n) = alloc n
dispatch (Slide n) = slide n
dispatch Add = arithmetic2 (+)
dispatch Sub = arithmetic2 (-)
dispatch Mul = arithmetic2 (*)
dispatch Div = arithmetic2 div
dispatch Neg = arithmetic1 negate
dispatch Eq = comparison (==)
dispatch Ne = comparison (/=)
dispatch Lt = comparison (<)
dispatch Le = comparison (<=)
dispatch Gt = comparison (>)
dispatch Ge = comparison (>=)
dispatch (Cond c1 c2) = cond c1 c2
dispatch Unwind = unwind
dispatch Eval = evalInstruction
#endif

cond :: GmCode -> GmCode -> GmState -> GmState
#if __CLH_EXERCISE_3__ < 40
cond c1 c2 state = putStack stack' (putCode code' state)
  where
    code'
      | v == 1 = c1 ++ code
      | v == 0 = c2 ++ code
      | otherwise = error "Condition is not 1 nor 0"
    NNum v = hLookup (getHeap state) a
    a : stack' = getStack state
    code = getCode state
#endif

#if __CLH_EXERCISE_3__ >= 27
#if __CLH_EXERCISE_3__ < 34
#if __CLH_EXERCISE_3__ < 31
compile program
  = (initialCode, [], [], heap, globals, statInitial)
  where
    (heap, globals) = buildInitialHeap program
#else
compile = undefined
#endif

initialCode = [PushGlobal "main", Eval]
#endif

#if __CLH_EXERCISE_3__ < 37
compiledPrimitives
  = [ ("+", 2, [Push 1, Eval, Push 1, Eval, Add, Update 2, Pop 2, Unwind])
    , ("-", 2, [Push 1, Eval, Push 1, Eval, Sub, Update 2, Pop 2, Unwind])
    , ("*", 2, [Push 1, Eval, Push 1, Eval, Mul, Update 2, Pop 2, Unwind])
    , ("/", 2, [Push 1, Eval, Push 1, Eval, Div, Update 2, Pop 2, Unwind])
    , ("negate", 1, [Push 0, Eval, Neg, Update 1, Pop 1, Unwind])
    , ("==", 2, [Push 1, Eval, Push 1, Eval, Eq, Update 2, Pop 2, Unwind])
    , ("~=", 2, [Push 1, Eval, Push 1, Eval, Ne, Update 2, Pop 2, Unwind])
    , ("<", 2, [Push 1, Eval, Push 1, Eval, Lt, Update 2, Pop 2, Unwind])
    , ("<=", 2, [Push 1, Eval, Push 1, Eval, Le, Update 2, Pop 2, Unwind])
    , (">", 2, [Push 1, Eval, Push 1, Eval, Gt, Update 2, Pop 2, Unwind])
    , (">=", 2, [Push 1, Eval, Push 1, Eval, Ge, Update 2, Pop 2, Unwind])
    , ("if", 3, [Push 0, Eval, Cond [Push 1] [Push 2], Update 3, Pop 3, Unwind])
    ]
#endif

#if __CLH_EXERCISE_3__ >= 28
#if __CLH_EXERCISE_3__ < 43
compileR e env = compileE e env ++ [Update d, Pop d, Unwind]
  where
    d = length env
#endif

compileE :: GmCompiler
#if __CLH_EXERCISE_3__ < 35
compileE (ENum n) env = [PushInt n]
compileE (ELet isRec defs e) env
  | isRec = compileLetRec compileE defs e env
  | otherwise = compileLet compileE defs e env
compileE (EAp (EAp (EVar name) e1) e2) env
  | name `elem` aDomain builtInDyadic = compileE e2 env ++ compileE e1 (argOffset 1 env) ++ [dyadic]
  where
    dyadic = aLookup builtInDyadic name (error "Invalid dyadic operator")
compileE (EAp (EVar "negate") e) env = compileE e env ++ [Neg]
compileE (EAp (EAp (EAp (EVar "if") e1) e2) e3) env = compileE e1 env ++ [Cond (compileE e2 env) (compileE e3 env)]
compileE e env = compileC e env ++ [Eval]
#endif

builtInDyadic :: Assoc Name Instruction
#if __CLH_EXERCISE_3__ < 41
builtInDyadic
  = [ ("+", Add), ("-", Sub), ("*", Mul), ("/", Div)
    , ("==", Eq), ("~=", Ne), ("<", Lt), ("<=", Le), (">", Gt), (">=", Ge)
    ]
#endif

#if __CLH_EXERCISE_3__ >= 29
#if __CLH_EXERCISE_3__ < 33
unwind state = newState (hLookup heap a)
  where
    stack@(a : as) = getStack state
    heap = getHeap state
    newState (NNum n)
      = case getDump state of
        (c, as') : dump' -> putDump dump' (putCode c (putStack (a : as') state))
        _ -> state
    newState (NAp a1 a2) = putCode [Unwind] (putStack (a1 : stack) state)
    newState (NGlobal n c)
      | length as < n
      = case getDump state of
          (c, as') : dump' -> putDump dump' (putCode c (putStack ((last stack) : as') state))
          _ -> error "Unwinding with too few arguments"
      | otherwise = putCode c (putStack (rearrange n heap stack) state)
    newState (NInd a') = putCode [Unwind] (putStack (a' : as) state)
#endif

#if __CLH_EXERCISE_3__ >= 31
#if __CLH_EXERCISE_3__ < 39
type GmState = (GmOutput, GmCode, GmStack, GmDump, GmHeap, GmGlobals, GmStats)
#endif

type GmOutput = String

getOutput :: GmState -> GmOutput
putOutput :: GmOutput -> GmState -> GmState
#if __CLH_EXERCISE_3__ < 39
getOutput (output, _, _, _, _, _, _) = output
putOutput output (_, code, stack, dump, heap, globals, stats) = (output, code, stack, dump, heap, globals, stats)

getDump (_, _, _, dump, _, _, _) = dump
putDump dump (output, code, stack, _, heap, globals, stats) = (output, code, stack, dump, heap, globals, stats)

getCode (_, code, _, _, _, _, _) = code
putCode code (output, _, stack, dump, heap, globals, stats) = (output, code, stack, dump, heap, globals, stats)

getStack (_, _, stack, _, _, _, _) = stack
putStack stack (output, code, _, dump, heap, globals, stats) = (output, code, stack, dump, heap, globals, stats)

getHeap (_, _, _, _, heap, _, _) = heap
putHeap heap (output, code, stack, dump, _, globals, stats) = (output, code, stack, dump, heap, globals, stats)

getGlobals (_, _, _, _, _, globals, _) = globals
putGlobals globals (output, code, stack, dump, heap, _, stats) = (output, code, stack, dump, heap, globals, stats)

getStats (_, _, _, _, _, _, stats) = stats
putStats stats (output, code, stack, dump, heap, globals, _) = (output, code, stack, dump, heap, globals, stats)
#endif

data Node
  = NNum Int
  | NAp Addr Addr
  | NGlobal Int GmCode
  | NInd Addr
  | NConstr Int [Addr]
  deriving (Show, Read, Eq)

#if __CLH_EXERCISE_3__ < 40
showState state
  = iConcat [ showOutput state, iNewline
            , showStack state, iNewline
            , showDump state, iNewline
            , showInstructions (getCode state), iNewline
            ]
#endif

showOutput :: GmState -> ISeq
showOutput state = iConcat [ iStr "  Output: \"", iStr (getOutput state), iStr "\"" ]

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

#if __CLH_EXERCISE_3__ >= 32
#if __CLH_EXERCISE_3__ < 40
data Instruction
  = Unwind
  | PushGlobal Name
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
  deriving (Show, Read, Eq)

showInstruction Unwind = iStr "Unwind"
showInstruction (PushGlobal f) = iStr "PushGlobal " `iAppend` iStr f
showInstruction (Push n) = iStr "Push " `iAppend` iNum n
showInstruction (PushInt n) = iStr "PushInt " `iAppend` iNum n
showInstruction MkAp = iStr "MkAp"
showInstruction (Update n) = iStr "Update " `iAppend` iNum n
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
showInstruction Print = iStr "Print"
#endif

showAlters :: Assoc Int GmCode -> ISeq
showAlters alters
  = iConcat [ iStr "[ ", iIndent (iInterleave iNewline (map showAlter alters)), iStr " ]"]

showAlter :: (Int, GmCode) -> ISeq
showAlter (tag, code)
  = iConcat [ iNum tag, iStr " -> [ ", iIndent (iInterleave iNewline (map showInstruction code)), iStr " ]" ]

#if __CLH_EXERCISE_3__ >= 33
#if __CLH_EXERCISE_3__ < 40
dispatch (PushGlobal f) = pushGlobal f
dispatch (PushInt n) = pushInt n
dispatch MkAp = mkAp
dispatch (Push n) = push n
dispatch (Update n) = update n
dispatch (Pop n) = pop n
dispatch (Alloc n) = alloc n
dispatch (Slide n) = slide n
dispatch Add = arithmetic2 (+)
dispatch Sub = arithmetic2 (-)
dispatch Mul = arithmetic2 (*)
dispatch Div = arithmetic2 div
dispatch Neg = arithmetic1 negate
dispatch Eq = comparison (==)
dispatch Ne = comparison (/=)
dispatch Lt = comparison (<)
dispatch Le = comparison (<=)
dispatch Gt = comparison (>)
dispatch Ge = comparison (>=)
dispatch (Cond c1 c2) = cond c1 c2
dispatch (Pack tag arity) = pack tag arity
dispatch (CaseJump alters) = caseJump alters
dispatch (Split arity) = split arity
dispatch Unwind = unwind
dispatch Eval = evalInstruction
dispatch Print = printInstruction
#endif

pack :: Int -> Int -> GmState -> GmState
pack tag arity state = putHeap heap' (putStack stack' state)
  where
    stack' = a : drop arity stack
    (heap', a) = hAlloc (getHeap state) (NConstr tag args)
    args = take arity stack
    stack = getStack state

caseJump :: Assoc Int GmCode -> GmState -> GmState
caseJump alters state = putCode code' state
  where
    code' = c' ++ getCode state
    c' = aLookup alters tag (error ("No case for constructor " ++ show tag))
    NConstr tag _ = hLookup (getHeap state) a
    a : _ = getStack state

split :: Int -> GmState -> GmState
split n state = putStack stack' state
  where
    stack' = args ++ as
    NConstr _ args = hLookup (getHeap state) a
    a : as = getStack state

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

#if __CLH_EXERCISE_3__ < 44
unwind state = newState (hLookup heap a)
  where
    stack@(a : as) = getStack state
    heap = getHeap state
    newState (NNum n)
      = case getDump state of
        (c, as') : dump' -> putDump dump' (putCode c (putStack (a : as') state))
        _ -> state
    newState (NAp a1 a2) = putCode [Unwind] (putStack (a1 : stack) state)
    newState (NGlobal n c)
      | length as < n
      = case getDump state of
          (c, as') : dump' -> putDump dump' (putCode c (putStack ((last stack) : as') state))
          _ -> error "Unwinding with too few arguments"
      | otherwise = putCode c (putStack (rearrange n heap stack) state)
    newState (NInd a') = putCode [Unwind] (putStack (a' : as) state)
    newState (NConstr _ _)
      = case getDump state of
        (c, as') : dump' -> putDump dump' (putCode c (putStack (a : as') state))
        _ -> state
#endif

#if __CLH_EXERCISE_3__ >= 34
compileAlters :: (Int -> GmCompiler) -> [CoreAlter] -> GmEnvironment -> Assoc Int GmCode
compileAlters comp alters env
  = [ (tag, comp (length names) body (zip names [0..] ++ argOffset (length names) env))
    | (tag, names, body) <- alters
    ]

compileE' :: Int -> GmCompiler
compileE' offset expr env
  = [Split offset] ++ compileE expr env ++ [Slide offset]

#if __CLH_EXERCISE_3__ < 41
#if __CLH_EXERCISE_3__ < 39
compile program
  = ([], initialCode, [], [], heap, globals, statInitial)
  where
    (heap, globals) = buildInitialHeap program
#else
compile = undefined
#endif
#endif

initialCode = [PushGlobal "main", Eval, Print]

#if __CLH_EXERCISE_3__ >= 35
#if __CLH_EXERCISE_3__ < 38
compileC (EVar v) env
  | v `elem` aDomain env = [Push vInd]
  | otherwise = [PushGlobal v]
  where
    vInd = aLookup env v (error "Can't happen")
compileC (ENum n) env = [PushInt n]
compileC (EConstr tag 0) env = [Pack tag 0]
compileC e@(EAp e1 e2) env
  | withEnoughArgs spine = compileCS (reverse spine) env
  | otherwise = compileC e2 env ++ compileC e1 (argOffset 1 env) ++ [MkAp]
  where
    spine = getSpine e
    withEnoughArgs (EConstr _ arity : es) = arity == length es
    withEnoughArgs _ = False
compileC (ELet isRec defs e) env
  | isRec = compileLetRec compileC defs e env
  | otherwise = compileLet compileC defs e env
#endif

getSpine :: CoreExpr -> [CoreExpr]
getSpine (EAp e1 e2) = getSpine e1 ++ [e2]
getSpine e = [e]

compileCS :: [CoreExpr] -> GmEnvironment -> [Instruction]
compileCS [EConstr tag arity] env = [Pack tag arity]
compileCS (e : es) env = compileC e env ++ compileCS es (argOffset 1 env)

#if __CLH_EXERCISE_3__ < 37
compileE (ENum n) env = [PushInt n]
compileE (ELet isRec defs e) env
  | isRec = compileLetRec compileE defs e env
  | otherwise = compileLet compileE defs e env
compileE (EAp (EAp (EVar name) e1) e2) env
  | name `elem` aDomain builtInDyadic = compileE e2 env ++ compileE e1 (argOffset 1 env) ++ [dyadic]
  where
    dyadic = aLookup builtInDyadic name (error "Invalid dyadic operator")
compileE (EAp (EVar "negate") e) env = compileE e env ++ [Neg]
compileE (EAp (EAp (EAp (EVar "if") e1) e2) e3) env = compileE e1 env ++ [Cond (compileE e2 env) (compileE e3 env)]
compileE (ECase e alters) env = compileE e env ++ [CaseJump (compileAlters compileE' alters env)]
compileE e env = compileC e env ++ [Eval]
#endif

#if __CLH_EXERCISE_3__ >= 37
#if __CLH_EXERCISE_3__ < 42
compileE (ENum n) env = [PushInt n]
compileE (ELet isRec defs e) env
  | isRec = compileLetRec compileE defs e env
  | otherwise = compileLet compileE defs e env
compileE (EAp (EAp (EVar name) e1) e2) env
  | name `elem` aDomain builtInDyadic = compileE e2 env ++ compileE e1 (argOffset 1 env) ++ [dyadic]
  where
    dyadic = aLookup builtInDyadic name (error "Invalid dyadic operator")
compileE (EAp (EVar "negate") e) env = compileE e env ++ [Neg]
compileE (ECase e alters) env = compileE e env ++ [CaseJump (compileAlters compileE' alters env)]
compileE e env = compileC e env ++ [Eval]
#endif

compiledPrimitives
  = [ ("+", 2, [Push 1, Eval, Push 1, Eval, Add, Update 2, Pop 2, Unwind])
    , ("-", 2, [Push 1, Eval, Push 1, Eval, Sub, Update 2, Pop 2, Unwind])
    , ("*", 2, [Push 1, Eval, Push 1, Eval, Mul, Update 2, Pop 2, Unwind])
    , ("/", 2, [Push 1, Eval, Push 1, Eval, Div, Update 2, Pop 2, Unwind])
    , ("negate", 1, [Push 0, Eval, Neg, Update 1, Pop 1, Unwind])
    , ("==", 2, [Push 1, Eval, Push 1, Eval, Eq, Update 2, Pop 2, Unwind])
    , ("~=", 2, [Push 1, Eval, Push 1, Eval, Ne, Update 2, Pop 2, Unwind])
    , ("<", 2, [Push 1, Eval, Push 1, Eval, Lt, Update 2, Pop 2, Unwind])
    , ("<=", 2, [Push 1, Eval, Push 1, Eval, Le, Update 2, Pop 2, Unwind])
    , (">", 2, [Push 1, Eval, Push 1, Eval, Gt, Update 2, Pop 2, Unwind])
    , (">=", 2, [Push 1, Eval, Push 1, Eval, Ge, Update 2, Pop 2, Unwind])
    , ("if", 3, [Push 0, Eval, CaseJump [(1, [Split 0, Push 2, Eval, Slide 0]), (2, [Split 0, Push 1, Eval, Slide 0])], Update 3, Pop 3, Unwind])
    ]

boxBoolean b state
  = putStack (a : getStack state) (putHeap heap' state)
  where
    (heap', a) = hAlloc (getHeap state) (NConstr tag [])
    tag
      | b = 2
      | otherwise = 1

#if __CLH_EXERCISE_3__ >= 38
pushGlobal name state
  | shouldBeAdded = putHeap heap' (putGlobals globals' (putStack stack' state))
  where
    stack' = a : getStack state
    globals' = (name, a) : getGlobals state
    (heap', a) = hAlloc (getHeap state) (NGlobal arity [Pack tag arity, Update 0, Unwind])
    ((tag, arity), _) : _ = parsedPack

    shouldBeAdded = not (null parsedPack || globalExists)

    globalExists = name `elem` aDomain (getGlobals state)
    parsedPack = readP_to_S parsePack name
    parsePack = do
      _ <- string "Pack{"
      tag <- readS_to_P (reads :: ReadS Int)
      _ <- string ","
      arity <- readS_to_P (reads :: ReadS Int)
      _ <- string "}"
      eof
      return (tag, arity)
pushGlobal name state = putStack (a : getStack state) state
  where
    a = aLookup (getGlobals state) name (error ("Undeclared global " ++ name))

compileC (EVar v) env
  | v `elem` aDomain env = [Push vInd]
  | otherwise = [PushGlobal v]
  where
    vInd = aLookup env v (error "Can't happen")
compileC (ENum n) env = [PushInt n]
compileC (EConstr tag arity) env = [PushGlobal ("Pack{" ++ show tag ++ "," ++ show arity ++ "}")]
compileC (EAp e1 e2) env = compileC e2 env ++ compileC e1 (argOffset 1 env) ++ [MkAp]
compileC (ELet isRec defs e) env
  | isRec = compileLetRec compileC defs e env ++ [Slide (length defs)]
  | otherwise = compileLet compileC defs e env ++ [Slide (length defs)]

#if __CLH_EXERCISE_3__ >= 39
type GmState = (GmOutput, GmCode, GmStack, GmDump, GmVStack, GmHeap, GmGlobals, GmStats)

type GmVStack = [Int]

getVStack :: GmState -> GmVStack
putVStack :: GmVStack -> GmState -> GmState
getVStack (_, _, _, _, vStack, _, _, _) = vStack
putVStack vStack (output, code, stack, dump, _, heap, globals, stats) = (output, code, stack, dump, vStack, heap, globals, stats)

getOutput (output, _, _, _, _, _, _, _) = output
putOutput output (_, code, stack, dump, vStack, heap, globals, stats) = (output, code, stack, dump, vStack, heap, globals, stats)

getDump (_, _, _, dump, _, _, _, _) = dump
putDump dump (output, code, stack, _, vStack, heap, globals, stats) = (output, code, stack, dump, vStack, heap, globals, stats)

getCode (_, code, _, _, _, _, _, _) = code
putCode code (output, _, stack, dump, vStack, heap, globals, stats) = (output, code, stack, dump, vStack, heap, globals, stats)

getStack (_, _, stack, _, _, _, _, _) = stack
putStack stack (output, code, _, dump, vStack, heap, globals, stats) = (output, code, stack, dump, vStack, heap, globals, stats)

getHeap (_, _, _, _, _, heap, _, _) = heap
putHeap heap (output, code, stack, dump, vStack, _, globals, stats) = (output, code, stack, dump, vStack, heap, globals, stats)

getGlobals (_, _, _, _, _, _, globals, _) = globals
putGlobals globals (output, code, stack, dump, vStack, heap, _, stats) = (output, code, stack, dump, vStack, heap, globals, stats)

getStats (_, _, _, _, _, _, _, stats) = stats
putStats stats (output, code, stack, dump, vStack, heap, globals, _) = (output, code, stack, dump, vStack, heap, globals, stats)

#if __CLH_EXERCISE_3__ >= 40
showState state
  = iConcat [ showOutput state, iNewline
            , showStack state, iNewline
            , showDump state, iNewline
            , showVStack state, iNewline
            , showInstructions (getCode state), iNewline
            ]

showVStack :: GmState -> ISeq
showVStack state
  = iConcat [ iStr "  VStack: [ ", iInterleave (iStr ",") (map iNum (getVStack state)), iStr " ]" ]

#if __CLH_EXERCISE_3__ < 46
data Instruction
  = Unwind
  | PushGlobal Name
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
  deriving (Show, Read, Eq)

showInstruction Unwind = iStr "Unwind"
showInstruction (PushGlobal f) = iStr "PushGlobal " `iAppend` iStr f
showInstruction (Push n) = iStr "Push " `iAppend` iNum n
showInstruction (PushInt n) = iStr "PushInt " `iAppend` iNum n
showInstruction MkAp = iStr "MkAp"
showInstruction (Update n) = iStr "Update " `iAppend` iNum n
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

dispatch (PushGlobal f) = pushGlobal f
dispatch (PushInt n) = pushInt n
dispatch (PushBasic n) = pushBasic n
dispatch MkAp = mkAp
dispatch MkBool = mkBool
dispatch MkInt = mkInt
dispatch Get = getInstruction
dispatch (Push n) = push n
dispatch (Update n) = update n
dispatch (Pop n) = pop n
dispatch (Alloc n) = alloc n
dispatch (Slide n) = slide n
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
dispatch (Cond c1 c2) = cond c1 c2
dispatch (Pack tag arity) = pack tag arity
dispatch (CaseJump alters) = caseJump alters
dispatch (Split arity) = split arity
dispatch Unwind = unwind
dispatch Eval = evalInstruction
dispatch Print = printInstruction
#endif

pushBasic :: Int -> GmState -> GmState
pushBasic n state = putVStack (n : getVStack state) state

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

dyadicBoolOp :: (Int -> Int -> Bool) -> GmState -> GmState
dyadicBoolOp op = dyadicIntOp ((convert .) . op)
  where
    convert True = 2
    convert False = 1

neg :: GmState -> GmState
neg state = putVStack ((negate n) : ns) state
  where
    n : ns = getVStack state

cond c1 c2 state = putCode code' (putVStack vStack' state)
  where
    code'
      | n == 2 = c1 ++ code
      | n == 1 = c2 ++ code
      | otherwise = error "Condition is not 2 nor 1"
    n : vStack' = getVStack state
    code = getCode state

#if __CLH_EXERCISE_3__ >= 41
compile program
  = ([], initialCode, [], [], [], heap, globals, statInitial)
  where
    (heap, globals) = buildInitialHeap program

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

builtInDyadicInt :: Assoc Name Instruction
builtInDyadicInt = [ ("+", Add), ("-", Sub), ("*", Mul), ("/", Div) ]

builtInDyadicBool :: Assoc Name Instruction
builtInDyadicBool = [ ("==", Eq), ("~=", Ne), ("<", Lt), ("<=", Le), (">", Gt), (">=", Ge) ]

builtInDyadic = builtInDyadicInt ++ builtInDyadicBool

#if __CLH_EXERCISE_3__ >= 42
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

#if __CLH_EXERCISE_3__ >= 43
#if __CLH_EXERCISE_3__ < 46
compileR (ELet isRec defs e) env
  | isRec = compileLetRec compileR defs e env
  | otherwise = compileLet compileR defs e env
compileR (EAp (EAp (EAp (EVar "if") e1) e2) e3) env
  = compileB e1 env ++ [Cond (compileR e2 env) (compileR e3 env)]
compileR (ECase e alters) env
  = compileE e env ++ [CaseJump (compileAlters compileR' alters env)]
compileR e env = compileE e env ++ [Update d, Pop d, Unwind]
  where
    d = length env
#endif

compileR' :: Int -> GmCompiler
compileR' offset expr env
  = [Split offset] ++ compileR expr env

#if __CLH_EXERCISE_3__ >= 44
type GmDumpItem = (GmCode, GmStack, GmVStack)

showDumpItem (code, stack, vStack)
  = iConcat [ iStr "<", shortShowInstructions 3 code
            , iStr ", ", shortShowStack stack
            , iStr ", ", shortShowVStack vStack, iStr ">"
            ]

shortShowVStack :: GmVStack -> ISeq
shortShowVStack vStack
  = iConcat [ iStr "[", iInterleave (iStr ", ") . map iNum $ vStack, iStr "]" ]

evalInstruction state = putCode [Unwind] (putStack [a] (putDump dump' state))
  where
    dump' = (code, as, getVStack state) : getDump state
    a : as = getStack state
    code = getCode state

unwind state = newState (hLookup heap a)
  where
    stack@(a : as) = getStack state
    heap = getHeap state
    newState (NNum n)
      = case getDump state of
        (c, as', vs') : dump' -> putDump dump' (putCode c (putStack (a : as') (putVStack vs' state)))
        _ -> state
    newState (NAp a1 a2) = putCode [Unwind] (putStack (a1 : stack) state)
    newState (NGlobal n c)
      | length as < n
      = case getDump state of
          (c, as', vs') : dump' -> putDump dump' (putCode c (putStack ((last stack) : as') (putVStack vs' state)))
          _ -> error "Unwinding with too few arguments"
      | otherwise = putCode c (putStack (rearrange n heap stack) state)
    newState (NInd a') = putCode [Unwind] (putStack (a' : as) state)
    newState (NConstr _ _)
      = case getDump state of
        (c, as', vs') : dump' -> putDump dump' (putCode c (putStack (a : as') (putVStack vs' state)))
        _ -> state

#if __CLH_EXERCISE_3__ >= 46
#if __CLH_EXERCISE_3__ < 47
data Instruction
  = Unwind
  | PushGlobal Name
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
  deriving (Show, Read, Eq)

showInstruction Unwind = iStr "Unwind"
showInstruction (PushGlobal f) = iStr "PushGlobal " `iAppend` iStr f
showInstruction (Push n) = iStr "Push " `iAppend` iNum n
showInstruction (PushInt n) = iStr "PushInt " `iAppend` iNum n
showInstruction MkAp = iStr "MkAp"
showInstruction (Update n) = iStr "Update " `iAppend` iNum n
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

dispatch (PushGlobal f) = pushGlobal f
dispatch (PushInt n) = pushInt n
dispatch (PushBasic n) = pushBasic n
dispatch MkAp = mkAp
dispatch MkBool = mkBool
dispatch MkInt = mkInt
dispatch Get = getInstruction
dispatch (Push n) = push n
dispatch (Update n) = update n
dispatch (Pop n) = pop n
dispatch (Alloc n) = alloc n
dispatch (Slide n) = slide n
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
dispatch (Cond c1 c2) = cond c1 c2
dispatch (Pack tag arity) = pack tag arity
dispatch (CaseJump alters) = caseJump alters
dispatch (Split arity) = split arity
dispatch Unwind = unwind
dispatch Eval = evalInstruction
dispatch Print = printInstruction
dispatch Return = returnInstruction
#endif

returnInstruction :: GmState -> GmState
returnInstruction state
  = putStack stack' (putDump dump' (putVStack vStack' (putCode code' state)))
  where
    stack' = last (getStack state) : as
    (code', as, vStack') : dump' = getDump state

#if __CLH_EXERCISE_3__ < 47
compileR (ELet isRec defs e) env
  | isRec = compileLetRec compileR defs e env
  | otherwise = compileLet compileR defs e env
compileR (EAp (EAp (EAp (EVar "if") e1) e2) e3) env
  = compileB e1 env ++ [Cond (compileR e2 env) (compileR e3 env)]
compileR (ECase e alters) env
  = compileE e env ++ [CaseJump (compileAlters compileR' alters env)]
compileR e@(ENum _) env = compileE e env ++ [Update (length env), Return]
compileR e@(EConstr _ _) env = compileE e env ++ [Update (length env), Return]
compileR e env = compileE e env ++ [Update d, Pop d, Unwind]
  where
    d = length env
#endif

#if __CLH_EXERCISE_3__ >= 47
data Instruction
  = Unwind
  | PushGlobal Name
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

showInstruction Unwind = iStr "Unwind"
showInstruction (PushGlobal f) = iStr "PushGlobal " `iAppend` iStr f
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

dispatch (PushGlobal f) = pushGlobal f
dispatch (PushInt n) = pushInt n
dispatch (PushBasic n) = pushBasic n
dispatch MkAp = mkAp
dispatch MkBool = mkBool
dispatch MkInt = mkInt
dispatch Get = getInstruction
dispatch (Push n) = push n
dispatch (Update n) = update n
dispatch (UpdateInt n) = updateInt n
dispatch (UpdateBool n) = updateBool n
dispatch (Pop n) = pop n
dispatch (Alloc n) = alloc n
dispatch (Slide n) = slide n
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
dispatch (Cond c1 c2) = cond c1 c2
dispatch (Pack tag arity) = pack tag arity
dispatch (CaseJump alters) = caseJump alters
dispatch (Split arity) = split arity
dispatch Unwind = unwind
dispatch Eval = evalInstruction
dispatch Print = printInstruction
dispatch Return = returnInstruction

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
