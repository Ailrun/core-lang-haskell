module Language.TIM where

import Data.ISeq
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

data Instruction
  = Take Int
  | Enter TimAddrMode
  | Push TimAddrMode

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

data TimValueStack = DummyTimValueStack

data TimDump = DummyTimDump

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

type TimStats = Int

statInitial = 0
statIncSteps s = s + 1
statGetSteps s = s

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
initialArgStack = []

initialValueStack :: TimValueStack
initialValueStack = DummyTimValueStack

initialDump :: TimDump
initialDump = DummyTimDump

compiledPrimitives :: CodeStore
compiledPrimitives = []

type TimCompilerEnv = Assoc Name TimAddrMode

compileSc :: TimCompilerEnv -> CoreScDefn -> (Name, [Instruction])
compileSc env (name, args, body)
  = (name, Take (length args) : instructions)
  where
    instructions = compileR body env'
    env' = zip args (map Arg [1..]) ++ env

compileR :: CoreExpr -> TimCompilerEnv -> [Instruction]
compileR (EAp e1 e2) env = Push (compileA e2 env) : compileR e1 env
compileR e@(EVar _) env = [Enter (compileA e env)]
compileR e@(ENum _) env = [Enter (compileA e env)]
compileR e env = error "compileR: can't do this yet"

compileA :: CoreExpr -> TimCompilerEnv -> TimAddrMode
compileA (EVar v) env = aLookup env v (error ("Unknown variable " ++ v))
compileA (ENum n) env = IntConst n
compileA e env = Code (compileR e env)

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

amToClosure :: TimAddrMode -> FramePtr -> TimHeap -> CodeStore -> Closure
amToClosure (Arg n) fPtr heap cStore = fGet heap fPtr n
amToClosure (Code inst) fPtr heap cStore = (inst, fPtr)
amToClosure (Label l) fPtr heap cStore = (codeLookup cStore l, fPtr)
amToClosure (IntConst n) fPtr heap cStore = (intCode, FrameInt n)

intCode :: [Instruction]
intCode = []

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
  = iConcat [ iStr "Arg stack:       [ ", closuresSeq, iStr " ]" ]
  where
    closuresSeq
      = iIndent (iInterleave iNewline (map showClosure stack))

showValueStack :: TimValueStack -> ISeq
showValueStack _ = iNil

showDump :: TimDump -> ISeq
showDump _ = iNil

showClosure :: Closure -> ISeq
showClosure (inst, fPtr)
  = iConcat [ iStr "(", showInstructions Terse inst, iStr ", ", showFramePtr fPtr, iStr ")" ]

showFramePtr :: FramePtr -> ISeq
showFramePtr FrameNull = iStr "null"
showFramePtr (FrameAddr addr) = iStr (showAddr addr)
showFramePtr (FrameInt n) = iStr "int " `iAppend` iNum n

showStats :: TimState -> ISeq
showStats (_, _, _, _, _, heap, _, stats)
  = iConcat [ iStr "Steps taken = ", iNum (statGetSteps stats), iNewline
            , iStr "No of frames allocated = ", iNum (hSize heap), iNewline
            ]

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
showInstruction _ (Take n) = iStr "Take " `iAppend` iNum n
showInstruction d (Enter x) = iStr "Enter " `iAppend` showArg d x
showInstruction d (Push x) = iStr "Push " `iAppend` showArg d x

showArg :: HowMuchToPrint -> TimAddrMode -> ISeq
showArg _ (Arg n) = iStr "Arg " `iAppend` iNum n
showArg d (Code inst) = iStr "Code " `iAppend` showInstructions d inst
showArg _ (Label s) = iStr "Label " `iAppend` iStr s
showArg _ (IntConst n) = iStr "IntConst " `iAppend` iNum n

nTerse :: Int
nTerse = 3
