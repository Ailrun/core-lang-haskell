{-# LANGUAGE CPP #-}
module Language.GMachine
  ( run
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

run :: String -> String
run = showResults . eval . compile . parse

type GmState = (GmCode, GmStack, GmHeap, GmGlobals, GmStats)

type GmCode = [Instruction]

getCode :: GmState -> GmCode
getCode (code, _, _, _, _) = code
putCode :: GmCode -> GmState -> GmState
putCode code (_, stack, heap, globals, stats) = (code, stack, heap, globals, stats)

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
getStack (_, stack, _, _, _) = stack
putStack :: GmStack -> GmState -> GmState
putStack stack (code, _, heap, globals, stats) = (code, stack, heap, globals, stats)

type GmHeap = Heap Node

getHeap :: GmState -> GmHeap
getHeap (_, _, heap, _, _) = heap
putHeap :: GmHeap -> GmState -> GmState
putHeap heap (code, stack, _, globals, stats) = (code, stack, heap, globals, stats)

#if __CLH_EXERCISE_3__ < 8
data Node
  = NNum Int
  | NAp Addr Addr
  | NGlobal Int GmCode
  deriving (Show, Read, Eq)
#endif

type GmGlobals = Assoc Name Addr

getGlobals :: GmState -> GmGlobals
getGlobals (_, _, _, globals, _) = globals
putGlobals :: GmGlobals -> GmState -> GmState
putGlobals globals (code, stack, heap, _, stats) = (code, stack, heap, globals, stats)

statInitial :: GmStats
statIncSteps :: GmStats -> GmStats
statGetSteps :: GmStats -> Int

type GmStats = Int

statInitial = 0
statIncSteps s = s + 1
statGetSteps s = s

getStats :: GmState -> GmStats
getStats (_, _, _, _, stats) = stats
putStats :: GmStats -> GmState -> GmState
putStats stats (code, stack, heap, globals, _) = (code, stack, heap, globals, stats)

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
pushGlobal name state = putStack (a : getStack state) state
  where
    a = aLookup (getGlobals state) name (error ("Undeclared global " ++ name))

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
push n state = putStack (a : as) state
  where
    as = getStack state
    a = getArg (hLookup (getHeap state) (as !! (n + 1)))

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
compile program = (initialCode, [], heap, globals, statInitial)
  where
    (heap, globals) = buildInitialHeap program

buildInitialHeap :: CoreProgram -> (GmHeap, GmGlobals)
buildInitialHeap program = mapAccumL allocateSc hInitial compiled
  where
    compiled = map compileSc (preludeDefs ++ program) ++ compiledPrimitives

type GmCompiledSc = (Name, Int, GmCode)

allocateSc :: GmHeap -> GmCompiledSc -> (GmHeap, (Name, Addr))
allocateSc heap (name, n, is) = (heap', (name, a))
  where
    (heap', a) = hAlloc heap (NGlobal n is)

initialCode :: GmCode
initialCode = [PushGlobal "main", Unwind]

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
compileC (EVar v) env
  | v `elem` aDomain env = [Push vInd]
  | otherwise = [PushGlobal v]
  where
    vInd = aLookup env v (error "Can't happen")
compileC (ENum n) env = [PushInt n]
compileC (EAp e1 e2) env = compileC e2 env ++ compileC e1 (argOffset 1 env) ++ [MkAp]

argOffset :: Int -> GmEnvironment -> GmEnvironment
argOffset n env = [ (v, m + n) | (v, m) <- env ]

compiledPrimitives :: [GmCompiledSc]
compiledPrimitives = []

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
            , iNewline
            ]
    where
      (NGlobal _ code) = hLookup (getHeap state) addr

showInstructions :: GmCode -> ISeq
showInstructions is
  = iConcat [ iStr "  Code: {", iIndent (iInterleave iNewline (map showInstruction is)), iStr "}", iNewline
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
showState state
  = iConcat [ showStack state, iNewline
            , showInstructions (getCode state), iNewline
            ]

showStack :: GmState -> ISeq
showStack state
  = iConcat [ iStr "  Stack: [", iIndent (iInterleave iNewline (map (showStackItem state) (reverse (getStack state)))), iStr "]"
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

#if __CLH_EXERCISE_3__ >= 8
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

#if __CLH_EXERCISE_3__ >= 9
dispatch (PushGlobal f) = pushGlobal f
dispatch (PushInt n) = pushInt n
dispatch MkAp = mkAp
dispatch (Push n) = push n
dispatch (Update n) = update n
dispatch (Pop n) = pop n
dispatch Unwind = unwind

update :: Int -> GmState -> GmState
update n state = putStack stack' (putHeap heap' state)
  where
    heap' = hUpdate (getHeap state) rA (NInd a)
    rA = stack' !! n
    a : stack' = getStack state

pop :: Int -> GmState -> GmState
pop n state = putStack (drop n (getStack state)) state

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

#if __CLH_EXERCISE_3__ >= 10
compileR e env = compileC e env ++ [Update d, Pop d, Unwind]
  where
    d = length env
#endif
#endif
#endif
#endif
#endif
