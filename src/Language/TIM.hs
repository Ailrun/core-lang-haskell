{-# LANGUAGE CPP #-}
module Language.TIM
  ( run
  , compile
  , eval
  , showResults
  , fullRun
  )
where

import Control.Arrow
import Data.ISeq
import Data.List
import Language.Parser
import Language.Prelude
import Language.Types
import Util

run :: String -> String
compile :: CoreProgram -> TimState
eval :: TimState -> [TimState]
showResults :: [TimState] -> String

run = showResults . eval . compile . parse

fullRun :: String -> String
fullRun = showFullResults . eval . compile . parse

#if __CLH_EXERCISE_4__ < 4
data Instruction
  = Take Int
  | Enter TimAddrMode
  | Push TimAddrMode
#endif

data TimAddrMode
  = Arg Int
  | Label Name
  | Code [Instruction]
  | IntConst Int

type TimState
  = ( [Instruction]
    , FramePtr
    , TimStack
    , TimValueStack
    , TimDump
    , TimHeap
    , CodeStore
    , TimStats
    )

data FramePtr
  = FrameAddr Addr
  | FrameInt Int
  | FrameNull

type TimStack = [Closure]
type Closure = ([Instruction], FramePtr)

#if __CLH_EXERCISE_4__ < 4
data TimValueStack = DummyTimValueStack
#endif

#if __CLH_EXERCISE_4__ < 16
data TimDump = DummyTimDump
#endif

type TimHeap = Heap Frame

fAlloc :: TimHeap -> [Closure] -> (TimHeap, FramePtr)
fGet :: TimHeap -> FramePtr -> Int -> Closure
fUpdate :: TimHeap -> FramePtr -> Int -> Closure -> TimHeap
fList :: Frame -> [Closure]

fAlloc heap xs = (heap', FrameAddr addr)
  where
    (heap', addr) = hAlloc heap xs

fGet heap (FrameAddr addr) n = f !! (n - 1)
  where
    f = hLookup heap addr

fUpdate heap (FrameAddr addr) n closure
  = hUpdate heap addr frame'
  where
    frame' = take (n - 1) frame ++ [closure] ++ drop n frame
    frame = hLookup heap addr

fList f = f

type Frame = [Closure]

type CodeStore = Assoc Name [Instruction]

codeLookup :: CodeStore -> Name -> [Instruction]
codeLookup cStore l
  = aLookup cStore l (error ("Attempt to jump to unknown label " ++ show l))

statInitial :: TimStats
statIncSteps :: TimStats -> TimStats
statGetSteps :: TimStats -> Int

#if __CLH_EXERCISE_4__ < 2
type TimStats = Int

statInitial = 0
statIncSteps s = s + 1
statGetSteps s = s
#endif

compile program
  = ( [Enter (Label "main")]
    , FrameNull
    , initialArgStack
    , initialValueStack
    , initialDump
    , hInitial
    , compiledCode
    , statInitial
    )
  where
    compiledCode = compiledScDefns ++ compiledPrimitives
    compiledScDefns = map (compileSc initialEnv) scDefns
    scDefns = preludeDefs ++ program
    initialEnv
      = [ (name, Label name) | (name, _, _) <- scDefns ]
      ++ [ (name, Label name) | (name, _) <- compiledPrimitives ]

initialArgStack :: TimStack
#if __CLH_EXERCISE_4__ < 4
initialArgStack = []
#endif

initialValueStack :: TimValueStack
#if __CLH_EXERCISE_4__ < 2
initialValueStack = DummyTimValueStack
#endif

initialDump :: TimDump
#if __CLH_EXERCISE_4__ < 16
initialDump = DummyTimDump
#endif

compiledPrimitives :: CodeStore
#if __CLH_EXERCISE_4__ < 4
compiledPrimitives = []
#endif

type TimCompilerEnv = Assoc Name TimAddrMode

compileSc :: TimCompilerEnv -> CoreScDefn -> (Name, [Instruction])
#if __CLH_EXERCISE_4__ < 2
compileSc env (name, args, body)
  = (name, Take (length args) : instructions)
  where
    instructions = compileR body env'
    env' = zip args (map Arg [1..]) ++ env
#endif

#if __CLH_EXERCISE_4__ < 11
compileR :: CoreExpr -> TimCompilerEnv -> [Instruction]
#endif
#if __CLH_EXERCISE_4__ < 6
compileR (EAp e1 e2) env = Push (compileA e2 env) : compileR e1 env
compileR e@(EVar _) env = [Enter (compileA e env)]
compileR e@(ENum _) env = [Enter (compileA e env)]
compileR e env = error "compileR: can't do this yet"
#endif

#if __CLH_EXERCISE_4__ < 11
compileA :: CoreExpr -> TimCompilerEnv -> TimAddrMode
compileA (EVar v) env = aLookup env v (error ("Unknown variable " ++ v))
compileA (ENum n) env = IntConst n
compileA e env = Code (compileR e env)
#endif

eval state = state : restState
  where
    restState
      | timFinal state = []
      | otherwise = eval nextState
    nextState = doAdmin (step state)

doAdmin :: TimState -> TimState
doAdmin = applyToStats statIncSteps

timFinal :: TimState -> Bool
timFinal ([], _, _, _, _, _, _, _) = True
timFinal _ = False

applyToStats :: (TimStats -> TimStats) -> TimState -> TimState
applyToStats statsFun (inst, fPtr, stack, vStack, dump, heap, cStore, stats)
  = (inst, fPtr, stack, vStack, dump, heap, cStore, statsFun stats)

step :: TimState -> TimState
#if __CLH_EXERCISE_4__ < 2
step (Take n : inst, fPtr, stack, vStack, dump, heap, cStore, stats)
  | length stack >= n = (inst, fPtr', drop n stack, vStack, dump, heap', cStore, stats)
  | otherwise = error "Too few args for Take instruction"
  where
    (heap', fPtr') = fAlloc heap (take n stack)
step ([Enter am], fPtr, stack, vStack, dump, heap, cStore, stats)
  = (inst', fPtr', stack, vStack, dump, heap, cStore, stats)
  where
    (inst', fPtr') = amToClosure am fPtr heap cStore
step (Push am : inst, fPtr, stack, vStack, dump, heap, cStore, stats)
  = (inst, fPtr, stack', vStack, dump, heap, cStore, stats)
  where
    stack' = amToClosure am fPtr heap cStore : stack
#endif

amToClosure :: TimAddrMode -> FramePtr -> TimHeap -> CodeStore -> Closure
amToClosure (Arg n) fPtr heap cStore = fGet heap fPtr n
amToClosure (Code inst) fPtr heap cStore = (inst, fPtr)
amToClosure (Label l) fPtr heap cStore = (codeLookup cStore l, fPtr)
amToClosure (IntConst n) fPtr heap cStore = (intCode, FrameInt n)

intCode :: [Instruction]
#if __CLH_EXERCISE_4__ < 4
intCode = []
#endif

showFullResults states = iDisplay fullResultsSeq
  where
    fullResultsSeq
      = iConcat [ iStr "Supercombinator definitions", iNewline
                , iNewline
                , showScDefns (head states), iNewline
                , iNewline
                , iStr "State transitions", iNewline
                , iLayn (map showState states), iNewline
                , iNewline
                , showStats (last states)
                ]

showResults states = iDisplay resultsSeq
  where
    resultsSeq
      = iConcat [ showState lastState, iNewline
                , iNewline
                , showStats lastState
                ]
    lastState = last states

showScDefns :: TimState -> ISeq
showScDefns (_, _, _, _, _, _, cStore, _)
  = iInterleave iNewline (map showSc cStore)

showSc :: (Name, [Instruction]) -> ISeq
showSc (name, inst)
  = iConcat [ iStr "Code for ", iStr name, iStr ":", iNewline
            , iStr "   ", showInstructions Full inst, iNewline
            , iNewline
            ]

showState :: TimState -> ISeq
showState (inst, fPtr, stack, vStack, dump, heap, _, _)
  = iConcat [ iStr "Code:            ", showInstructions Terse inst, iNewline
            , showFrame heap fPtr
            , showStack stack
            , showValueStack vStack
            , showDump dump
            , iNewline
            ]

showFrame :: TimHeap -> FramePtr -> ISeq
showFrame heap FrameNull = iStr "Null frame ptr" `iAppend` iNewline
showFrame heap (FrameAddr addr)
  = iConcat [ iStr "Frame:           < ", closuresSeq, iStr " >", iNewline ]
  where
    closuresSeq
      = iIndent (iInterleave iNewline (map showClosure (fList (hLookup heap addr))))
showFrame heap (FrameInt n)
  = iConcat [ iStr "Frame ptr (int): ", iNum n, iNewline ]

showStack :: TimStack -> ISeq
showStack stack
  = iConcat [ iStr "Arg stack:       [ ", closuresSeq, iStr " ]", iNewline ]
  where
    closuresSeq
      = iIndent (iInterleave iNewline (map showClosure stack))

showValueStack :: TimValueStack -> ISeq
#if __CLH_EXERCISE_4__ < 4
showValueStack _ = iNil
#endif

showDump :: TimDump -> ISeq
#if __CLH_EXERCISE_4__ < 16
showDump _ = iNil
#endif

showClosure :: Closure -> ISeq
showClosure (inst, fPtr)
  = iConcat [ iStr "(", showInstructions Terse inst, iStr ", ", showFramePtr fPtr, iStr ")" ]

showFramePtr :: FramePtr -> ISeq
showFramePtr FrameNull = iStr "null"
showFramePtr (FrameAddr addr) = iStr (showAddr addr)
showFramePtr (FrameInt n) = iStr "int " `iAppend` iNum n

showStats :: TimState -> ISeq
#if __CLH_EXERCISE_4__ < 2
showStats (_, _, _, _, _, heap, _, stats)
  = iConcat [ iStr "Steps taken = ", iNum (statGetSteps stats), iNewline
            , iStr "No of frames allocated = ", iNum (hSize heap), iNewline
            ]
#endif

data HowMuchToPrint
  = Full
  | Terse
  | None

showInstructions :: HowMuchToPrint -> [Instruction] -> ISeq
showInstructions None _ = iStr "{ .. }"
showInstructions Terse inst
  = iConcat [ iStr "{ ", iIndent (iInterleave (iStr ", ") body), iStr " }" ]
  where
    instSeq = map (showInstruction None) inst
    body
      | length inst <= nTerse = instSeq
      | otherwise = take nTerse instSeq ++ [iStr ".."]
showInstructions Full inst
  = iConcat [ iStr "{ ", iIndent (iInterleave sep instSeq), iStr " }" ]
  where
    sep = iStr "," `iAppend` iNewline
    instSeq = map (showInstruction Full) inst

showInstruction :: HowMuchToPrint -> Instruction -> ISeq
#if __CLH_EXERCISE_4__ < 4
showInstruction _ (Take n) = iStr "Take " `iAppend` iNum n
showInstruction d (Enter x) = iStr "Enter " `iAppend` showArg d x
showInstruction d (Push x) = iStr "Push " `iAppend` showArg d x
#endif

showArg :: HowMuchToPrint -> TimAddrMode -> ISeq
showArg _ (Arg n) = iStr "Arg " `iAppend` iNum n
showArg d (Code inst) = iStr "Code " `iAppend` showInstructions d inst
showArg _ (Label s) = iStr "Label " `iAppend` iStr s
showArg _ (IntConst n) = iStr "IntConst " `iAppend` iNum n

nTerse :: Int
nTerse = 3

#if __CLH_EXERCISE_4__ >= 2
-- |
-- Number of heap allocation is omitted
-- because I'm not sure how this heap with frames can be
-- compared directly with the heap of the template instantiation machine.
type TimStats = (Int, Int, Int)

statInitial = (0, 0, 0)
statIncSteps (step, time, maxStackDepth) = (step + 1, time, maxStackDepth)
statGetSteps (step, _, _) = step

statSpendTime :: Int -> TimStats -> TimStats
statSpendTime n (step, time, maxStackDepth) = (step, time + n, maxStackDepth)

statGetTime :: TimStats -> Int
statGetTime (_, time, _) = time

statUpdateMaxStackDepth :: TimStack -> TimStats -> TimStats
statUpdateMaxStackDepth stack stats@(step, time, maxStackDepth)
  | n > maxStackDepth = (step, time, n)
  | otherwise = stats
  where
    n = length stack

statGetMaxStackDepth :: TimStats -> Int
statGetMaxStackDepth (_, _, maxStackDepth) = maxStackDepth

#if __CLH_EXERCISE_4__ < 3
step (Take n : inst, fPtr, stack, vStack, dump, heap, cStore, stats)
  | length stack >= n = (inst, fPtr', drop n stack, vStack, dump, heap', cStore, statSpendTime (n + 1) stats)
  | otherwise = error "Too few args for Take instruction"
  where
    (heap', fPtr') = fAlloc heap (take n stack)
step ([Enter am], fPtr, stack, vStack, dump, heap, cStore, stats)
  = (inst', fPtr', stack, vStack, dump, heap, cStore, statSpendTime 1 stats)
  where
    (inst', fPtr') = amToClosure am fPtr heap cStore
step (Push am : inst, fPtr, stack, vStack, dump, heap, cStore, stats)
  = (inst, fPtr, stack', vStack, dump, heap, cStore, (statUpdateMaxStackDepth stack' . statSpendTime 1) stats)
  where
    stack' = amToClosure am fPtr heap cStore : stack
#endif

showStats (_, _, _, _, _, heap, _, stats)
  = iConcat [ iStr "Steps taken = ", iNum (statGetSteps stats), iNewline
            , iStr "Execution time = ", iNum (statGetTime stats), iNewline
            , iStr "Max depth of the stack = ", iNum (statGetMaxStackDepth stats), iNewline
            , iStr "No of frames allocated = ", iNum (hSize heap), iNewline
            ]

#if __CLH_EXERCISE_4__ >= 3
#if __CLH_EXERCISE_4__ < 11
compileSc env (name, args, body)
  = case args of
      [] -> (name, instructions)
      _ -> (name, Take (length args) : instructions)
  where
    instructions = compileR body env'
    env' = zip args (map Arg [1..]) ++ env
#endif

#if __CLH_EXERCISE_4__ >= 4
type TimValueStack = [Int]

initialValueStack = []

#if __CLH_EXERCISE_4__ < 11
data Instruction
  = Take Int
  | Enter TimAddrMode
  | Push TimAddrMode
  | PushV ValueAMode
  | Return
  | Op Op
  | Cond [Instruction] [Instruction]
#endif

data Op
  = Add | Sub | Mul | Div
  | Neg
  | Gt | Ge | Lt | Le | Eq | Ne
  deriving (Eq)

data ValueAMode
  = FramePtr
  | IntVConst Int

#if __CLH_EXERCISE_4__ < 11
showInstruction _ (Take n) = iStr "Take " `iAppend` iNum n
showInstruction d (Enter x) = iStr "Enter " `iAppend` showArg d x
showInstruction d (Push x) = iStr "Push " `iAppend` showArg d x
showInstruction _ (PushV FramePtr) = iStr "PushV FramePtr"
showInstruction _ (PushV (IntVConst n)) = iStr "PushV " `iAppend` iNum n
showInstruction _ Return = iStr "Return"
showInstruction _ (Op op) = iStr "Op " `iAppend` showOp op
showInstruction None (Cond c1 c2)
  = iConcat [ iStr "Cond ", showInstructions None c1, iStr " ", showInstructions None c2 ]
showInstruction d (Cond c1 c2)
  = iConcat [ iStr "Cond "
            , iIndent (iConcat [showInstructions d c1, iNewline, showInstructions d c2])
            ]
#endif

showOp :: Op -> ISeq
showOp Add = iStr "Add"
showOp Sub = iStr "Sub"
showOp Mul = iStr "Mul"
showOp Div = iStr "Div"
showOp Neg = iStr "Neg"
showOp Gt = iStr "Gt"
showOp Ge = iStr "Ge"
showOp Lt = iStr "Lt"
showOp Le = iStr "Le"
showOp Eq = iStr "Eq"
showOp Ne = iStr "Ne"

#if __CLH_EXERCISE_4__ < 5
step (Take n : inst, fPtr, stack, vStack, dump, heap, cStore, stats)
  | length stack >= n = (inst, fPtr', drop n stack, vStack, dump, heap', cStore, statSpendTime (n + 1) stats)
  | otherwise = error "Too few args for Take instruction"
  where
    (heap', fPtr') = fAlloc heap (take n stack)
step ([Enter am], fPtr, stack, vStack, dump, heap, cStore, stats)
  = (inst', fPtr', stack, vStack, dump, heap, cStore, statSpendTime 1 stats)
  where
    (inst', fPtr') = amToClosure am fPtr heap cStore
step (Push am : inst, fPtr, stack, vStack, dump, heap, cStore, stats)
  = (inst, fPtr, stack', vStack, dump, heap, cStore, (statUpdateMaxStackDepth stack' . statSpendTime 1) stats)
  where
    stack' = amToClosure am fPtr heap cStore : stack
step (PushV vMode : inst, fPtr, stack, vStack, dump, heap, cStore, stats)
  = case vMode of
      FramePtr ->
        case fPtr of
          FrameInt n -> (inst, fPtr, stack, n : vStack, dump, heap, cStore, statSpendTime 1 stats)
          _ -> error "Invalid frame pointer for PushV FramePtr"
      _ -> error "Not yet implemented"
step ([Return], fPtr, stack, vStack, dump, heap, cStore, stats)
  = case stack of
      (inst', fPtr') : stack' -> (inst', fPtr', stack', vStack, dump, heap, cStore, statSpendTime 1 stats)
      _ -> error "Invalid stack to Return"
step (Op op : inst, fPtr, stack, vStack, dump, heap, cStore, stats)
  | op `elem` aDomain binaryOpToFun
  = case vStack of
      n1 : n2 : ns -> (inst, fPtr, stack, binF n1 n2 : ns, dump, heap, cStore, (statSpendTime 1) stats)
      _ -> error ("Not enough values for the operation " ++ iDisplay (showOp op))
  | op `elem` aDomain unaryOpToFun
  = case vStack of
      n : ns -> (inst, fPtr, stack, unF n : ns, dump, heap, cStore, (statSpendTime 1) stats)
      _ -> error ("Not enough values for the operation " ++ iDisplay (showOp op))
  where
    unF = aLookup unaryOpToFun op (error (iDisplay (showOp op) ++ " is not a unary operator"))
    binF = aLookup binaryOpToFun op (error (iDisplay (showOp op) ++ " is not a binary operator"))
#endif

binaryOpToFun :: Assoc Op (Int -> Int -> Int)
#if __CLH_EXERCISE_4__ < 8
binaryOpToFun
  = [ (Add, (+))
    , (Sub, (-))
    , (Mul, (*))
    , (Div, div)
    ]
#endif

unaryOpToFun :: Assoc Op (Int -> Int)
unaryOpToFun
  = [ (Neg, negate) ]

#if __CLH_EXERCISE_4__ < 5
compiledPrimitives
  = [ ("+", [ Take 2, Push (Code [ Push (Code [ Op Add, Return ]), Enter (Arg 1) ]), Enter (Arg 2) ])
    , ("-", [ Take 2, Push (Code [ Push (Code [ Op Sub, Return ]), Enter (Arg 1) ]), Enter (Arg 2) ])
    , ("*", [ Take 2, Push (Code [ Push (Code [ Op Mul, Return ]), Enter (Arg 1) ]), Enter (Arg 2) ])
    , ("/", [ Take 2, Push (Code [ Push (Code [ Op Div, Return ]), Enter (Arg 1) ]), Enter (Arg 2) ])

    , ("negate", [ Take 1, Push (Code [ Op Neg, Return ]), Enter (Arg 1) ])
    ]
#endif

intCode
  = [ PushV FramePtr
    , Return
    ]

initialArgStack = [([], FrameNull)]

showValueStack vStack
  = iConcat [ iStr "Value stack:     { ", iInterleave (iStr ", ") (map iNum vStack), iStr " }", iNewline ]

#if __CLH_EXERCISE_4__ >= 5
#if __CLH_EXERCISE_4__ < 6
step (Take n : inst, fPtr, stack, vStack, dump, heap, cStore, stats)
  | length stack >= n = (inst, fPtr', drop n stack, vStack, dump, heap', cStore, statSpendTime (n + 1) stats)
  | otherwise = error "Too few args for Take instruction"
  where
    (heap', fPtr') = fAlloc heap (take n stack)
step ([Enter am], fPtr, stack, vStack, dump, heap, cStore, stats)
  = (inst', fPtr', stack, vStack, dump, heap, cStore, statSpendTime 1 stats)
  where
    (inst', fPtr') = amToClosure am fPtr heap cStore
step (Push am : inst, fPtr, stack, vStack, dump, heap, cStore, stats)
  = (inst, fPtr, stack', vStack, dump, heap, cStore, (statUpdateMaxStackDepth stack' . statSpendTime 1) stats)
  where
    stack' = amToClosure am fPtr heap cStore : stack
step (PushV vMode : inst, fPtr, stack, vStack, dump, heap, cStore, stats)
  = case vMode of
      FramePtr ->
        case fPtr of
          FrameInt n -> (inst, fPtr, stack, n : vStack, dump, heap, cStore, statSpendTime 1 stats)
          _ -> error "Invalid frame pointer for PushV FramePtr"
      _ -> error "Not yet implemented"
step ([Return], fPtr, stack, vStack, dump, heap, cStore, stats)
  = case stack of
      (inst', fPtr') : stack' -> (inst', fPtr', stack', vStack, dump, heap, cStore, statSpendTime 1 stats)
      _ -> error "Invalid stack to Return"
step (Op op : inst, fPtr, stack, vStack, dump, heap, cStore, stats)
  | op `elem` aDomain binaryOpToFun
  = case vStack of
      n1 : n2 : ns -> (inst, fPtr, stack, binF n1 n2 : ns, dump, heap, cStore, (statSpendTime 1) stats)
      _ -> error ("Not enough values for the operation " ++ iDisplay (showOp op))
  | op `elem` aDomain unaryOpToFun
  = case vStack of
      n : ns -> (inst, fPtr, stack, unF n : ns, dump, heap, cStore, (statSpendTime 1) stats)
      _ -> error ("Not enough values for the operation " ++ iDisplay (showOp op))
  where
    unF = aLookup unaryOpToFun op (error (iDisplay (showOp op) ++ " is not a unary operator"))
    binF = aLookup binaryOpToFun op (error (iDisplay (showOp op) ++ " is not a binary operator"))
step ([Cond inst1 inst2], fPtr, stack, vStack, dump, heap, cStore, stats)
  = case vStack of
      0 : vStack' -> (inst1, fPtr, stack, vStack', dump, heap, cStore, statSpendTime 1 stats)
      _ : vStack' -> (inst2, fPtr, stack, vStack', dump, heap, cStore, statSpendTime 1 stats)
      _ -> error "Invalid vStack for Cond"
#endif

#if __CLH_EXERCISE_4__ < 8
compiledPrimitives
  = [ ("+", [ Take 2, Push (Code [ Push (Code [ Op Add, Return ]), Enter (Arg 1) ]), Enter (Arg 2) ])
    , ("-", [ Take 2, Push (Code [ Push (Code [ Op Sub, Return ]), Enter (Arg 1) ]), Enter (Arg 2) ])
    , ("*", [ Take 2, Push (Code [ Push (Code [ Op Mul, Return ]), Enter (Arg 1) ]), Enter (Arg 2) ])
    , ("/", [ Take 2, Push (Code [ Push (Code [ Op Div, Return ]), Enter (Arg 1) ]), Enter (Arg 2) ])

    , ("negate", [ Take 1, Push (Code [ Op Neg, Return ]), Enter (Arg 1) ])

    , ("if", [ Take 3, Push (Code [ Cond [Enter (Arg 2)] [Enter (Arg 3)] ]), Enter (Arg 1) ])
    ]
#endif

#if __CLH_EXERCISE_4__ >= 6
#if __CLH_EXERCISE_4__ < 7
compileR e@(EAp _ _) env
  | isArithmeticExpr e = compileB e env [Return]
compileR e@(ENum _) env = compileB e env [Return]
compileR (EAp e1 e2) env = Push (compileA e2 env) : compileR e1 env
compileR e@(EVar _) env = [Enter (compileA e env)]
compileR e env = error "compileR: can't do this yet"
#endif

isArithmeticExpr :: CoreExpr -> Bool
isArithmeticExpr (EAp (EVar "negate") _) = True
isArithmeticExpr (EAp (EAp (EVar name) _) _) = name `elem` aDomain binaryNameToOp
isArithmeticExpr _ = False

binaryNameToOp :: Assoc String Op
#if __CLH_EXERCISE_4__ < 8
binaryNameToOp
  = [ ("+", Add), ("-", Sub), ("*", Mul), ("/", Div)
    ]
#endif

#if __CLH_EXERCISE_4__ < 11
compileB :: CoreExpr -> TimCompilerEnv -> [Instruction] -> [Instruction]
compileB e env cont
  | isArithmeticExpr e
  = case e of
      EAp (EVar name) e ->
        compileB e env (Op Neg : cont)
      EAp (EAp (EVar name) e1) e2 ->
        let
          cont' = Op (getOp name) : cont
        in
          compileB e2 env (compileB e1 env cont')
  where
    getOp name = aLookup binaryNameToOp name (error (name ++ " is not a binary operator"))
compileB (ENum n) env cont = PushV (IntVConst n) : cont
compileB e env cont = Push (Code cont) : compileR e env
#endif

#if __CLH_EXERCISE_4__ < 11
step (Take n : inst, fPtr, stack, vStack, dump, heap, cStore, stats)
  | length stack >= n = (inst, fPtr', drop n stack, vStack, dump, heap', cStore, statSpendTime (n + 1) stats)
  | otherwise = error "Too few args for Take instruction"
  where
    (heap', fPtr') = fAlloc heap (take n stack)
step ([Enter am], fPtr, stack, vStack, dump, heap, cStore, stats)
  = (inst', fPtr', stack, vStack, dump, heap, cStore, statSpendTime 1 stats)
  where
    (inst', fPtr') = amToClosure am fPtr heap cStore
step (Push am : inst, fPtr, stack, vStack, dump, heap, cStore, stats)
  = (inst, fPtr, stack', vStack, dump, heap, cStore, (statUpdateMaxStackDepth stack' . statSpendTime 1) stats)
  where
    stack' = amToClosure am fPtr heap cStore : stack
step (PushV vMode : inst, fPtr, stack, vStack, dump, heap, cStore, stats)
  = case vMode of
      FramePtr ->
        case fPtr of
          FrameInt n -> (inst, fPtr, stack, n : vStack, dump, heap, cStore, statSpendTime 1 stats)
          _ -> error "Invalid frame pointer for PushV FramePtr"
      IntVConst n -> (inst, fPtr, stack, n : vStack, dump, heap, cStore, statSpendTime 1 stats)
step ([Return], fPtr, stack, vStack, dump, heap, cStore, stats)
  = case stack of
      (inst', fPtr') : stack' -> (inst', fPtr', stack', vStack, dump, heap, cStore, statSpendTime 1 stats)
      _ -> error "Invalid stack to Return"
step (Op op : inst, fPtr, stack, vStack, dump, heap, cStore, stats)
  | op `elem` aDomain binaryOpToFun
  = case vStack of
      n1 : n2 : ns -> (inst, fPtr, stack, binF n1 n2 : ns, dump, heap, cStore, (statSpendTime 1) stats)
      _ -> error ("Not enough values for the operation " ++ iDisplay (showOp op))
  | op `elem` aDomain unaryOpToFun
  = case vStack of
      n : ns -> (inst, fPtr, stack, unF n : ns, dump, heap, cStore, (statSpendTime 1) stats)
      _ -> error ("Not enough values for the operation " ++ iDisplay (showOp op))
  where
    unF = aLookup unaryOpToFun op (error (iDisplay (showOp op) ++ " is not a unary operator"))
    binF = aLookup binaryOpToFun op (error (iDisplay (showOp op) ++ " is not a binary operator"))
step ([Cond inst1 inst2], fPtr, stack, vStack, dump, heap, cStore, stats)
  = case vStack of
      0 : vStack' -> (inst1, fPtr, stack, vStack', dump, heap, cStore, statSpendTime 1 stats)
      _ : vStack' -> (inst2, fPtr, stack, vStack', dump, heap, cStore, statSpendTime 1 stats)
      _ -> error "Invalid vStack for Cond"
#endif

#if __CLH_EXERCISE_4__ >= 7
#if __CLH_EXERCISE_4__ < 11
compileR e@(EAp _ _) env
  | isArithmeticExpr e = compileB e env [Return]
compileR e@(ENum _) env = compileB e env [Return]
compileR (EAp (EAp (EAp (EVar "if") e1) e2) e3) env
  = compileB e1 env [Cond (compileR e2 env) (compileR e3 env)]
compileR (EAp e1 e2) env = Push (compileA e2 env) : compileR e1 env
compileR e@(EVar _) env = [Enter (compileA e env)]
compileR e env = error "compileR: can't do this yet"
#endif

#if __CLH_EXERCISE_4__ >= 8
binaryOpToFun
  = [ (Add, (+))
    , (Sub, (-))
    , (Mul, (*))
    , (Div, div)
    , (Gt, boolBinaryFunToIntBinaryFun (>))
    , (Ge, boolBinaryFunToIntBinaryFun (>=))
    , (Lt, boolBinaryFunToIntBinaryFun (<))
    , (Le, boolBinaryFunToIntBinaryFun (<=))
    , (Eq, boolBinaryFunToIntBinaryFun (==))
    , (Ne, boolBinaryFunToIntBinaryFun (/=))
    ]

boolBinaryFunToIntBinaryFun :: (Int -> Int -> Bool) -> Int -> Int -> Int
boolBinaryFunToIntBinaryFun f n1 n2
  = if f n1 n2
    then 0
    else 1

#if __CLH_EXERCISE_4__ < 11
compiledPrimitives
  = [ ("+", [ Take 2, Push (Code [ Push (Code [ Op Add, Return ]), Enter (Arg 1) ]), Enter (Arg 2) ])
    , ("-", [ Take 2, Push (Code [ Push (Code [ Op Sub, Return ]), Enter (Arg 1) ]), Enter (Arg 2) ])
    , ("*", [ Take 2, Push (Code [ Push (Code [ Op Mul, Return ]), Enter (Arg 1) ]), Enter (Arg 2) ])
    , ("/", [ Take 2, Push (Code [ Push (Code [ Op Div, Return ]), Enter (Arg 1) ]), Enter (Arg 2) ])

    , ("negate", [ Take 1, Push (Code [ Op Neg, Return ]), Enter (Arg 1) ])

    , ("if", [ Take 3, Push (Code [ Cond [Enter (Arg 2)] [Enter (Arg 3)] ]), Enter (Arg 1) ])

    , (">", [ Take 2, Push (Code [ Push (Code [ Op Gt, Return ]), Enter (Arg 1) ]), Enter (Arg 2) ])
    , (">=", [ Take 2, Push (Code [ Push (Code [ Op Ge, Return ]), Enter (Arg 1) ]), Enter (Arg 2) ])
    , ("<", [ Take 2, Push (Code [ Push (Code [ Op Lt, Return ]), Enter (Arg 1) ]), Enter (Arg 2) ])
    , ("<=", [ Take 2, Push (Code [ Push (Code [ Op Le, Return ]), Enter (Arg 1) ]), Enter (Arg 2) ])
    , ("==", [ Take 2, Push (Code [ Push (Code [ Op Eq, Return ]), Enter (Arg 1) ]), Enter (Arg 2) ])
    , ("~=", [ Take 2, Push (Code [ Push (Code [ Op Ne, Return ]), Enter (Arg 1) ]), Enter (Arg 2) ])
    ]
#endif

binaryNameToOp
  = [ ("+", Add), ("-", Sub), ("*", Mul), ("/", Div)
    , (">", Gt), (">=", Ge), ("<", Lt), ("<=", Le), ("==", Eq), ("~=", Ne)
    ]

#if __CLH_EXERCISE_4__ >= 11
#if __CLH_EXERCISE_4__ < 16
data Instruction
  = Take Int Int
  | Enter TimAddrMode
  | Push TimAddrMode
  | PushV ValueAMode
  | Return
  | Op Op
  | Cond [Instruction] [Instruction]
  | Move Int TimAddrMode

showInstruction _ (Take t n)
  = iConcat [ iStr "Take ", iNum t, iStr " ", iNum n ]
showInstruction d (Enter x) = iStr "Enter " `iAppend` showArg d x
showInstruction d (Push x) = iStr "Push " `iAppend` showArg d x
showInstruction _ (PushV FramePtr) = iStr "PushV FramePtr"
showInstruction _ (PushV (IntVConst n)) = iStr "PushV " `iAppend` iNum n
showInstruction _ Return = iStr "Return"
showInstruction _ (Op op) = iStr "Op " `iAppend` showOp op
showInstruction None (Cond c1 c2)
  = iConcat [ iStr "Cond ", showInstructions None c1, iStr " ", showInstructions None c2 ]
showInstruction d (Cond c1 c2)
  = iConcat [ iStr "Cond "
            , iIndent (iConcat [showInstructions d c1, iNewline, showInstructions d c2])
            ]
showInstruction d (Move i x)
  = iConcat [ iStr "Move ", iNum i, showArg d x ]

step (Take t n : inst, fPtr, stack, vStack, dump, heap, cStore, stats)
  | length stack >= n = (inst, fPtr', drop n stack, vStack, dump, heap', cStore, statSpendTime (t + 1) stats)
  | otherwise = error "Too few args for Take instruction"
  where
    (heap', fPtr') = fAlloc heap closures
    closures = take n stack ++ replicate (t - n) ([], FrameNull)
step ([Enter am], fPtr, stack, vStack, dump, heap, cStore, stats)
  = (inst', fPtr', stack, vStack, dump, heap, cStore, statSpendTime 1 stats)
  where
    (inst', fPtr') = amToClosure am fPtr heap cStore
step (Push am : inst, fPtr, stack, vStack, dump, heap, cStore, stats)
  = (inst, fPtr, stack', vStack, dump, heap, cStore, (statUpdateMaxStackDepth stack' . statSpendTime 1) stats)
  where
    stack' = amToClosure am fPtr heap cStore : stack
step (PushV vMode : inst, fPtr, stack, vStack, dump, heap, cStore, stats)
  = case vMode of
      FramePtr ->
        case fPtr of
          FrameInt n -> (inst, fPtr, stack, n : vStack, dump, heap, cStore, statSpendTime 1 stats)
          _ -> error "Invalid frame pointer for PushV FramePtr"
      IntVConst n -> (inst, fPtr, stack, n : vStack, dump, heap, cStore, statSpendTime 1 stats)
step ([Return], fPtr, stack, vStack, dump, heap, cStore, stats)
  = case stack of
      (inst', fPtr') : stack' -> (inst', fPtr', stack', vStack, dump, heap, cStore, statSpendTime 1 stats)
      _ -> error "Invalid stack to Return"
step (Op op : inst, fPtr, stack, vStack, dump, heap, cStore, stats)
  | op `elem` aDomain binaryOpToFun
  = case vStack of
      n1 : n2 : ns -> (inst, fPtr, stack, binF n1 n2 : ns, dump, heap, cStore, (statSpendTime 1) stats)
      _ -> error ("Not enough values for the operation " ++ iDisplay (showOp op))
  | op `elem` aDomain unaryOpToFun
  = case vStack of
      n : ns -> (inst, fPtr, stack, unF n : ns, dump, heap, cStore, (statSpendTime 1) stats)
      _ -> error ("Not enough values for the operation " ++ iDisplay (showOp op))
  where
    unF = aLookup unaryOpToFun op (error (iDisplay (showOp op) ++ " is not a unary operator"))
    binF = aLookup binaryOpToFun op (error (iDisplay (showOp op) ++ " is not a binary operator"))
step ([Cond inst1 inst2], fPtr, stack, vStack, dump, heap, cStore, stats)
  = case vStack of
      0 : vStack' -> (inst1, fPtr, stack, vStack', dump, heap, cStore, statSpendTime 1 stats)
      _ : vStack' -> (inst2, fPtr, stack, vStack', dump, heap, cStore, statSpendTime 1 stats)
      _ -> error "Invalid vStack for Cond"
step (Move i am : inst, fPtr, stack, vStack, dump, heap, cStore, stats)
  = (inst, fPtr, stack, vStack, dump, heap', cStore, statSpendTime 1 stats)
  where
    heap' = fUpdate heap fPtr i (amToClosure am fPtr heap cStore)

compiledPrimitives
  = [ ("+", [ Take 2 2, Push (Code [ Push (Code [ Op Add, Return ]), Enter (Arg 1) ]), Enter (Arg 2) ])
    , ("-", [ Take 2 2, Push (Code [ Push (Code [ Op Sub, Return ]), Enter (Arg 1) ]), Enter (Arg 2) ])
    , ("*", [ Take 2 2, Push (Code [ Push (Code [ Op Mul, Return ]), Enter (Arg 1) ]), Enter (Arg 2) ])
    , ("/", [ Take 2 2, Push (Code [ Push (Code [ Op Div, Return ]), Enter (Arg 1) ]), Enter (Arg 2) ])

    , ("negate", [ Take 1 1, Push (Code [ Op Neg, Return ]), Enter (Arg 1) ])

    , ("if", [ Take 3 3, Push (Code [ Cond [Enter (Arg 2)] [Enter (Arg 3)] ]), Enter (Arg 1) ])

    , (">", [ Take 2 2, Push (Code [ Push (Code [ Op Gt, Return ]), Enter (Arg 1) ]), Enter (Arg 2) ])
    , (">=", [ Take 2 2, Push (Code [ Push (Code [ Op Ge, Return ]), Enter (Arg 1) ]), Enter (Arg 2) ])
    , ("<", [ Take 2 2, Push (Code [ Push (Code [ Op Lt, Return ]), Enter (Arg 1) ]), Enter (Arg 2) ])
    , ("<=", [ Take 2 2, Push (Code [ Push (Code [ Op Le, Return ]), Enter (Arg 1) ]), Enter (Arg 2) ])
    , ("==", [ Take 2 2, Push (Code [ Push (Code [ Op Eq, Return ]), Enter (Arg 1) ]), Enter (Arg 2) ])
    , ("~=", [ Take 2 2, Push (Code [ Push (Code [ Op Ne, Return ]), Enter (Arg 1) ]), Enter (Arg 2) ])
    ]

compileSc env (name, args, body)
  | d > 0 = (name, Take d argsLength : instructions)
  | otherwise = (name, instructions)
  where
    (d, instructions) = compileR body env' argsLength
    env' = zip args (map Arg [1..]) ++ env
    argsLength = length args
#endif

compileR :: CoreExpr -> TimCompilerEnv -> Int -> (Int, [Instruction])
#if __CLH_EXERCISE_4__ < 13
compileR e@(EAp _ _) env d
  | isArithmeticExpr e = compileB e env d [Return]
compileR e@(ENum _) env d = compileB e env d [Return]
compileR (ELet isRec defs eBody) env d
  | not isRec = (d', moveDefs ++ inst)
  | otherwise = error "Not implemented yet"
  where
    (d', inst) = compileR eBody env' dn
    env' = map (fst *** Arg) defWithSlots ++ env
    (dn, moveDefs) = mapAccumL makeMoveFromDef lastSlotForDefs defWithSlots

    makeMoveFromDef dDef ((_, eDef), slot)
      = second (Move slot) (compileA eDef env dDef)

    defWithSlots = zip defs [d + 1..lastSlotForDefs]
    lastSlotForDefs = d + length defs
compileR (EAp (EAp (EAp (EVar "if") e1) e2) e3) env d
  = compileB e1 env d3 [Cond inst2 inst3]
  where
    (d2, inst2) = compileR e2 env d
    (d3, inst3) = compileR e3 env d2
compileR (EAp e1 e2) env d = (d2, Push am : inst)
  where
    (d1, am) = compileA e2 env d
    (d2, inst) = compileR e1 env d1
compileR e@(EVar _) env d = (d', [Enter am])
  where
    (d', am) = compileA e env d
compileR e env d = error "compileR: can't do this yet"
#endif

compileA :: CoreExpr -> TimCompilerEnv -> Int -> (Int, TimAddrMode)
compileA (EVar v) env d = (d, aLookup env v (error ("Unknown variable " ++ v)))
compileA (ENum n) env d = (d, IntConst n)
compileA e env d = (d', Code inst)
  where
    (d', inst) = compileR e env d

compileB :: CoreExpr -> TimCompilerEnv -> Int -> [Instruction] -> (Int, [Instruction])
#if __CLH_EXERCISE_4__ < 15
compileB e env d cont
  | isArithmeticExpr e
  = case e of
      EAp (EVar name) e ->
        compileB e env d (Op Neg : cont)
      EAp (EAp (EVar name) e1) e2 ->
        let
          (d', cont') = compileB e1 env d (Op (getOp name) : cont)
        in
          compileB e2 env d' cont'
  where
    getOp name = aLookup binaryNameToOp name (error (name ++ " is not a binary operator"))
compileB (ENum n) env d cont = (d, PushV (IntVConst n) : cont)
compileB e env d cont = (d', Push (Code cont) : inst)
  where
    (d', inst) = compileR e env d
#endif

#if __CLH_EXERCISE_4__ >= 13
#if __CLH_EXERCISE_4__ < 14
compileR e@(EAp _ _) env d
  | isArithmeticExpr e = compileB e env d [Return]
compileR e@(ENum _) env d = compileB e env d [Return]
compileR (ELet isRec defs eBody) env d
  = (d', moveDefs ++ inst)
  where
    (d', inst) = compileR eBody env' dn
    env' = map (fst *** Arg) defWithSlots ++ env
    (dn, moveDefs) = mapAccumL makeMoveFromDef lastSlotForDefs defWithSlots

    makeMoveFromDef dDef ((_, eDef), slot)
      = second (Move slot) (compileA eDef defEnv dDef)

    defEnv
      | isRec = env'
      | otherwise = env

    defWithSlots = zip defs [d + 1..lastSlotForDefs]
    lastSlotForDefs = d + length defs
compileR (EAp (EAp (EAp (EVar "if") e1) e2) e3) env d
  = compileB e1 env d3 [Cond inst2 inst3]
  where
    (d2, inst2) = compileR e2 env d
    (d3, inst3) = compileR e3 env d2
compileR (EAp e1 e2) env d = (d2, Push am : inst)
  where
    (d1, am) = compileA e2 env d
    (d2, inst) = compileR e1 env d1
compileR e@(EVar _) env d = (d', [Enter am])
  where
    (d', am) = compileA e env d
compileR e env d = error "compileR: can't do this yet"
#endif

#if __CLH_EXERCISE_4__ >= 14
#if __CLH_EXERCISE_4__ < 15
compileR e@(EAp _ _) env d
  | isArithmeticExpr e = compileB e env d [Return]
compileR e@(ENum _) env d = compileB e env d [Return]
compileR (ELet isRec defs eBody) env d
  = (d', moveDefs ++ inst)
  where
    (d', inst) = compileR eBody env' dn
    env' = map (fst *** mkIndMode) defWithSlots ++ env
    (dn, moveDefs) = mapAccumL makeMoveFromDef lastSlotForDefs defWithSlots

    makeMoveFromDef dDef ((_, eDef), slot)
      = second (Move slot) (compileA eDef defEnv dDef)

    defEnv
      | isRec = env'
      | otherwise = env

    defWithSlots = zip defs [d + 1..lastSlotForDefs]
    lastSlotForDefs = d + length defs
compileR (EAp (EAp (EAp (EVar "if") e1) e2) e3) env d
  = compileB e1 env d3 [Cond inst2 inst3]
  where
    (d2, inst2) = compileR e2 env d
    (d3, inst3) = compileR e3 env d2
compileR (EAp e1 e2) env d = (d2, Push am : inst)
  where
    (d1, am) = compileA e2 env d
    (d2, inst) = compileR e1 env d1
compileR e@(EVar _) env d = (d', [Enter am])
  where
    (d', am) = compileA e env d
compileR e env d = error "compileR: can't do this yet"
#endif

mkIndMode :: Int -> TimAddrMode
mkIndMode = Code . return . Enter . Arg

#if __CLH_EXERCISE_4__ >= 15
#if __CLH_EXERCISE_4__ < 16
compileR e@(EAp _ _) env d
  | isArithmeticExpr e = compileB e env d [Return]
compileR e@(ENum _) env d = compileB e env d [Return]
compileR (ELet isRec defs eBody) env d
  = (d', moveDefs ++ inst)
  where
    (d', inst) = compileR eBody env' dn
    env' = map (fst *** mkIndMode) defWithSlots ++ env
    (dn, moveDefs) = mapAccumL makeMoveFromDef lastSlotForDefs defWithSlots

    makeMoveFromDef dDef ((_, eDef), slot)
      = second (Move slot) (compileA eDef defEnv dDef)

    defEnv
      | isRec = env'
      | otherwise = env

    defWithSlots = zip defs [d + 1..lastSlotForDefs]
    lastSlotForDefs = d + length defs
compileR (EAp (EAp (EAp (EVar "if") e1) e2) e3) env d
  = compileB e1 env (max d2 d3) [Cond inst2 inst3]
  where
    (d2, inst2) = compileR e2 env d
    (d3, inst3) = compileR e3 env d2
compileR (EAp e1 e2) env d = (d2, Push am : inst)
  where
    (d1, am) = compileA e2 env d
    (d2, inst) = compileR e1 env d1
compileR e@(EVar _) env d = (d', [Enter am])
  where
    (d', am) = compileA e env d
compileR e env d = error "compileR: can't do this yet"
#endif

compileB e env d cont
  | isArithmeticExpr e
  = case e of
      EAp (EVar name) e ->
        compileB e env d (Op Neg : cont)
      EAp (EAp (EVar name) e1) e2 ->
        let
          (d1, cont') = compileB e1 env d (Op (getOp name) : cont)
          (d2, inst) = compileB e2 env d cont'
        in
          (max d1 d2, inst)
  where
    getOp name = aLookup binaryNameToOp name (error (name ++ " is not a binary operator"))
compileB (ENum n) env d cont = (d, PushV (IntVConst n) : cont)
compileB e env d cont = (d', Push (Code cont) : inst)
  where
    (d', inst) = compileR e env d

#if __CLH_EXERCISE_4__ >= 16
type TimDump = [(FramePtr, Int, TimStack)]

initialDump = []

showDump dump
  = iConcat [ iStr "Dump:            [ "
            , iIndent (iInterleave iNewline (map showDumpItem dump))
            , iStr " ]"
            ]

showDumpItem :: (FramePtr, Int, TimStack) -> ISeq
showDumpItem (fPtr, slot, stack)
  = iConcat [ iStr "< ", showFramePtr fPtr, iStr ", ", iNum slot, iStr ", ", showShortStack 3 stack, iStr " >" ]

showShortStack :: Int -> TimStack -> ISeq
showShortStack limit stack
  = iConcat [ iStr "[ ", iIndent (iInterleave (iStr ", ") shortClosureSeqs), iStr " ]" ]
  where
    closureSeqs = map showClosure stack
    shortClosureSeqs
      | length stack > limit = take limit closureSeqs ++ [ iStr "..." ]
      | otherwise = closureSeqs

data Instruction
  = Take Int Int
  | Enter TimAddrMode
  | Push TimAddrMode
  | PushV ValueAMode
  | Return
  | Op Op
  | Cond [Instruction] [Instruction]
  | Move Int TimAddrMode
  | PushMarker Int

showInstruction _ (Take t n)
  = iConcat [ iStr "Take ", iNum t, iStr " ", iNum n ]
showInstruction d (Enter x) = iStr "Enter " `iAppend` showArg d x
showInstruction d (Push x) = iStr "Push " `iAppend` showArg d x
showInstruction _ (PushV FramePtr) = iStr "PushV FramePtr"
showInstruction _ (PushV (IntVConst n)) = iStr "PushV " `iAppend` iNum n
showInstruction _ Return = iStr "Return"
showInstruction _ (Op op) = iStr "Op " `iAppend` showOp op
showInstruction None (Cond c1 c2)
  = iConcat [ iStr "Cond ", showInstructions None c1, iStr " ", showInstructions None c2 ]
showInstruction d (Cond c1 c2)
  = iConcat [ iStr "Cond "
            , iIndent (iConcat [showInstructions d c1, iNewline, showInstructions d c2])
            ]
showInstruction d (Move i x)
  = iConcat [ iStr "Move ", iNum i, iStr " ", showArg d x ]
showInstruction _ (PushMarker n) = iStr "PushMarker " `iAppend` iNum n

step (Take t n : inst, fPtr, stack, vStack, dump, heap, cStore, stats)
  | length stack >= n = (inst, fPtr', drop n stack, vStack, dump, heap', cStore, statSpendTime (t + 1) stats)
  | otherwise = error "Too few args for Take instruction"
  where
    (heap', fPtr') = fAlloc heap closures
    closures = take n stack ++ replicate (t - n) ([], FrameNull)
step ([Enter am], fPtr, stack, vStack, dump, heap, cStore, stats)
  = (inst', fPtr', stack, vStack, dump, heap, cStore, statSpendTime 1 stats)
  where
    (inst', fPtr') = amToClosure am fPtr heap cStore
step (Push am : inst, fPtr, stack, vStack, dump, heap, cStore, stats)
  = (inst, fPtr, stack', vStack, dump, heap, cStore, (statUpdateMaxStackDepth stack' . statSpendTime 1) stats)
  where
    stack' = amToClosure am fPtr heap cStore : stack
step (PushV vMode : inst, fPtr, stack, vStack, dump, heap, cStore, stats)
  = case vMode of
      FramePtr ->
        case fPtr of
          FrameInt n -> (inst, fPtr, stack, n : vStack, dump, heap, cStore, statSpendTime 1 stats)
          _ -> error "Invalid frame pointer for PushV FramePtr"
      IntVConst n -> (inst, fPtr, stack, n : vStack, dump, heap, cStore, statSpendTime 1 stats)
step (inst@[Return], fPtr, [], vStack@(n : _), (fPtrU, i, stack') : dump', heap, cStore, stats)
  = (inst, fPtr, stack', vStack, dump', heap', cStore, statSpendTime 1 stats)
  where
    heap' = fUpdate heap fPtrU i (intCode, FrameInt n)
step ([Return], fPtr, stack, vStack, dump, heap, cStore, stats)
  = case stack of
      (inst', fPtr') : stack' -> (inst', fPtr', stack', vStack, dump, heap, cStore, statSpendTime 1 stats)
      _ -> error "Invalid state to Return"
step (Op op : inst, fPtr, stack, vStack, dump, heap, cStore, stats)
  | op `elem` aDomain binaryOpToFun
  = case vStack of
      n1 : n2 : ns -> (inst, fPtr, stack, binF n1 n2 : ns, dump, heap, cStore, (statSpendTime 1) stats)
      _ -> error ("Not enough values for the operation " ++ iDisplay (showOp op))
  | op `elem` aDomain unaryOpToFun
  = case vStack of
      n : ns -> (inst, fPtr, stack, unF n : ns, dump, heap, cStore, (statSpendTime 1) stats)
      _ -> error ("Not enough values for the operation " ++ iDisplay (showOp op))
  where
    unF = aLookup unaryOpToFun op (error (iDisplay (showOp op) ++ " is not a unary operator"))
    binF = aLookup binaryOpToFun op (error (iDisplay (showOp op) ++ " is not a binary operator"))
step ([Cond inst1 inst2], fPtr, stack, vStack, dump, heap, cStore, stats)
  = case vStack of
      0 : vStack' -> (inst1, fPtr, stack, vStack', dump, heap, cStore, statSpendTime 1 stats)
      _ : vStack' -> (inst2, fPtr, stack, vStack', dump, heap, cStore, statSpendTime 1 stats)
      _ -> error "Invalid vStack for Cond"
step (Move i am : inst, fPtr, stack, vStack, dump, heap, cStore, stats)
  = (inst, fPtr, stack, vStack, dump, heap', cStore, statSpendTime 1 stats)
  where
    heap' = fUpdate heap fPtr i (amToClosure am fPtr heap cStore)
step (PushMarker i : inst, fPtr, stack, vStack, dump, heap, cStore, stats)
  = (inst, fPtr, [], vStack, (fPtr, i, stack) : dump, heap, cStore, statSpendTime 1 stats)

#if __CLH_EXERCISE_4__ < 17
compiledPrimitives
  = [ ("+", [ Take 2 2, Push (Code [ Push (Code [ Op Add, Return ]), Enter (mkUpdIndMode 1) ]), Enter (mkUpdIndMode 2) ])
    , ("-", [ Take 2 2, Push (Code [ Push (Code [ Op Sub, Return ]), Enter (mkUpdIndMode 1) ]), Enter (mkUpdIndMode 2) ])
    , ("*", [ Take 2 2, Push (Code [ Push (Code [ Op Mul, Return ]), Enter (mkUpdIndMode 1) ]), Enter (mkUpdIndMode 2) ])
    , ("/", [ Take 2 2, Push (Code [ Push (Code [ Op Div, Return ]), Enter (mkUpdIndMode 1) ]), Enter (mkUpdIndMode 2) ])

    , ("negate", [ Take 1 1, Push (Code [ Op Neg, Return ]), Enter (mkUpdIndMode 1) ])

    , ("if", [ Take 3 3, Push (Code [ Cond [Enter (mkUpdIndMode 2)] [Enter (mkUpdIndMode 3)] ]), Enter (mkUpdIndMode 1) ])

    , (">", [ Take 2 2, Push (Code [ Push (Code [ Op Gt, Return ]), Enter (mkUpdIndMode 1) ]), Enter (mkUpdIndMode 2) ])
    , (">=", [ Take 2 2, Push (Code [ Push (Code [ Op Ge, Return ]), Enter (mkUpdIndMode 1) ]), Enter (mkUpdIndMode 2) ])
    , ("<", [ Take 2 2, Push (Code [ Push (Code [ Op Lt, Return ]), Enter (mkUpdIndMode 1) ]), Enter (mkUpdIndMode 2) ])
    , ("<=", [ Take 2 2, Push (Code [ Push (Code [ Op Le, Return ]), Enter (mkUpdIndMode 1) ]), Enter (mkUpdIndMode 2) ])
    , ("==", [ Take 2 2, Push (Code [ Push (Code [ Op Eq, Return ]), Enter (mkUpdIndMode 1) ]), Enter (mkUpdIndMode 2) ])
    , ("~=", [ Take 2 2, Push (Code [ Push (Code [ Op Ne, Return ]), Enter (mkUpdIndMode 1) ]), Enter (mkUpdIndMode 2) ])
    ]
#endif

compileSc env (name, args, body)
  | d > 0 = (name, Take d argsLength : instructions)
  | otherwise = (name, instructions)
  where
    (d, instructions) = compileR body env' argsLength
    env' = zip args (map mkUpdIndMode [1..]) ++ env
    argsLength = length args

#if __CLH_EXERCISE_4__ < 17
compileR e@(EAp _ _) env d
  | isArithmeticExpr e = compileB e env d [Return]
compileR e@(ENum _) env d = compileB e env d [Return]
compileR (ELet isRec defs eBody) env d
  = (d', moveDefs ++ inst)
  where
    (d', inst) = compileR eBody env' dn
    env' = map (fst *** mkUpdIndMode) defWithSlots ++ env
    (dn, moveDefs) = mapAccumL makeMoveFromDef lastSlotForDefs defWithSlots

    makeMoveFromDef dDef ((_, eDef), slot)
      = second (Move slot) (compileA eDef defEnv dDef)

    defEnv
      | isRec = env'
      | otherwise = env

    defWithSlots = zip defs [d + 1..lastSlotForDefs]
    lastSlotForDefs = d + length defs
compileR (EAp (EAp (EAp (EVar "if") e1) e2) e3) env d
  = compileB e1 env (max d2 d3) [Cond inst2 inst3]
  where
    (d2, inst2) = compileR e2 env d
    (d3, inst3) = compileR e3 env d2
compileR (EAp e1 e2) env d = (d2, Push am : inst)
  where
    (d1, am) = compileA e2 env d
    (d2, inst) = compileR e1 env d1
compileR e@(EVar _) env d = (d', [Enter am])
  where
    (d', am) = compileA e env d
compileR e env d = error "compileR: can't do this yet"
#endif

mkUpdIndMode :: Int -> TimAddrMode
mkUpdIndMode n = Code [PushMarker n, Enter (Arg n)]

#if __CLH_EXERCISE_4__ >= 17
compiledPrimitives
  = [ ("+", [ Take 2 2, Push (Code [ Push (Code [ Op Add, Return ]), PushMarker 1, Enter (Arg 1) ]), PushMarker 2, Enter (Arg 2) ])
    , ("-", [ Take 2 2, Push (Code [ Push (Code [ Op Sub, Return ]), PushMarker 1, Enter (Arg 1) ]), PushMarker 2, Enter (Arg 2) ])
    , ("*", [ Take 2 2, Push (Code [ Push (Code [ Op Mul, Return ]), PushMarker 1, Enter (Arg 1) ]), PushMarker 2, Enter (Arg 2) ])
    , ("/", [ Take 2 2, Push (Code [ Push (Code [ Op Div, Return ]), PushMarker 1, Enter (Arg 1) ]), PushMarker 2, Enter (Arg 2) ])

    , ("negate", [ Take 1 1, Push (Code [ Op Neg, Return ]), PushMarker 1, Enter (Arg 1) ])

    , ("if", [ Take 3 3, Push (Code [ Cond [PushMarker 2, Enter (Arg 2)] [PushMarker 3, Enter (Arg 3)] ]), PushMarker 1, Enter (Arg 1) ])

    , (">", [ Take 2 2, Push (Code [ Push (Code [ Op Gt, Return ]), PushMarker 1, Enter (Arg 1) ]), PushMarker 2, Enter (Arg 2) ])
    , (">=", [ Take 2 2, Push (Code [ Push (Code [ Op Ge, Return ]), PushMarker 1, Enter (Arg 1) ]), PushMarker 2, Enter (Arg 2) ])
    , ("<", [ Take 2 2, Push (Code [ Push (Code [ Op Lt, Return ]), PushMarker 1, Enter (Arg 1) ]), PushMarker 2, Enter (Arg 2) ])
    , ("<=", [ Take 2 2, Push (Code [ Push (Code [ Op Le, Return ]), PushMarker 1, Enter (Arg 1) ]), PushMarker 2, Enter (Arg 2) ])
    , ("==", [ Take 2 2, Push (Code [ Push (Code [ Op Eq, Return ]), PushMarker 1, Enter (Arg 1) ]), PushMarker 2, Enter (Arg 2) ])
    , ("~=", [ Take 2 2, Push (Code [ Push (Code [ Op Ne, Return ]), PushMarker 1, Enter (Arg 1) ]), PushMarker 2, Enter (Arg 2) ])
    ]

#if __CLH_EXERCISE_4__ < 18
compileR e@(EAp _ _) env d
  | isArithmeticExpr e = compileB e env d [Return]
compileR e@(ENum _) env d = compileB e env d [Return]
compileR (ELet isRec defs eBody) env d
  = (d', moveDefs ++ inst)
  where
    (d', inst) = compileR eBody env' dn
    env' = map (fst *** mkUpdIndMode) defWithSlots ++ env
    (dn, moveDefs) = mapAccumL makeMoveFromDef lastSlotForDefs defWithSlots

    makeMoveFromDef dDef ((_, eDef), slot)
      = second (Move slot) (compileA eDef defEnv dDef)

    defEnv
      | isRec = env'
      | otherwise = env

    defWithSlots = zip defs [d + 1..lastSlotForDefs]
    lastSlotForDefs = d + length defs
compileR (EAp (EAp (EAp (EVar "if") e1) e2) e3) env d
  = compileB e1 env (max d2 d3) [Cond inst2 inst3]
  where
    (d2, inst2) = compileR e2 env d
    (d3, inst3) = compileR e3 env d2
compileR (EAp e1 e2) env d = (d2, Push am : inst)
  where
    (d1, am) = compileA e2 env d
    (d2, inst) = compileR e1 env d1
compileR e@(EVar _) env d = (d', mkEnter am)
  where
    (d', am) = compileA e env d
compileR e env d = error "compileR: can't do this yet"
#endif

mkEnter :: TimAddrMode -> [Instruction]
mkEnter (Code i) = i
mkEnter am = [Enter am]

#if __CLH_EXERCISE_4__ >= 18
compileR e@(EAp _ _) env d
  | isArithmeticExpr e = compileB e env d [Return]
compileR e@(ENum _) env d = compileB e env d [Return]
compileR (ELet isRec defs eBody) env d
  = (d', moveDefs ++ inst)
  where
    (d', inst) = compileR eBody env' dn
    env' = map (fst *** mkIndMode) defWithSlots ++ env
    (dn, moveDefs) = mapAccumL makeMoveFromDef lastSlotForDefs defWithSlots

    makeMoveFromDef dDef ((_, eDef), slot)
      = second (Move slot) (compileU eDef slot defEnv dDef)

    defEnv
      | isRec = env'
      | otherwise = env

    defWithSlots = zip defs [d + 1..lastSlotForDefs]
    lastSlotForDefs = d + length defs
compileR (EAp (EAp (EAp (EVar "if") e1) e2) e3) env d
  = compileB e1 env (max d2 d3) [Cond inst2 inst3]
  where
    (d2, inst2) = compileR e2 env d
    (d3, inst3) = compileR e3 env d2
compileR (EAp e1 e2) env d = (d2, Push am : inst)
  where
    (d1, am) = compileA e2 env d
    (d2, inst) = compileR e1 env d1
compileR e@(EVar _) env d = (d', mkEnter am)
  where
    (d', am) = compileA e env d
compileR e env d = error "compileR: can't do this yet"

#if __CLH_EXERCISE_4__ < 19
compileU :: CoreExpr -> Int -> TimCompilerEnv -> Int -> (Int, TimAddrMode)
compileU e slot env d = (d', Code (PushMarker slot : inst))
  where
    (d', inst) = compileR e env d
#endif

#if __CLH_EXERCISE_4__ >= 19
compileU :: CoreExpr -> Int -> TimCompilerEnv -> Int -> (Int, TimAddrMode)
compileU (ENum n) slot env d = (d, IntConst n)
compileU e slot env d = (d', Code (PushMarker slot : inst))
  where
    (d', inst) = compileR e env d
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
