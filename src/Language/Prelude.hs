{-# LANGUAGE CPP #-}
module Language.Prelude where

import Language.Types

preludeDefs :: CoreProgram
preludeDefs
  = [ ("I", ["x"], EVar "x")
    , ("K", ["x", "y"], EVar "x")
    , ("K1", ["x", "y"], EVar "y")
    , ("S", ["f", "g", "x"], EAp (EAp (EVar "f") (EVar "x")) (EAp (EVar "g") (EVar "x")))
    , ("compose", ["f", "g", "x"], EAp (EVar "f") (EAp (EVar "g") (EVar "x")))
    , ("twice", ["f"], EAp (EAp (EVar "compose") (EVar "f")) (EVar "f"))
#if __CLH_EXERCISE_2__ >= 24
    , ("Cons", [], EConstr 2 2)
    , ("Nil", [], EConstr 1 0)
    , ("head", ["l"], EAp (EAp (EAp (EVar "caseList") (EVar "l")) (EVar "abort")) (EVar "K"))
    , ("tail", ["l"], EAp (EAp (EAp (EVar "caseList") (EVar "l")) (EVar "abort")) (EVar "K1"))
#endif
    ]
