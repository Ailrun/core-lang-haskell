```
compileSc ("S" ["f", "g", "x"], EAp (EAp (EVar "f") (EVar "x")) (EAp (EVar "g") (EVar "x")))
=> ("S", 3, compileR (EAp (EAp (EVar "f") (EVar "x")) (EAp (EVar "g") (EVar "x"))) [("f", 0), ("g", 1), ("x", 2)])
=> ("S", 3, compileC (EAp (EAp (EVar "f") (EVar "x")) (EAp (EVar "g") (EVar "x"))) [("f", 0), ("g", 1), ("x", 2)] ++ [Slide 4, Unwind])
=> ("S", 3, compileC (EAp (EVar "f") (EVar "x")) [("f", 0), ("g", 1), ("x", 2)] ++ compileC (EAp (EVar "g") (EVar "x")) [("f", 1), ("g", 2), ("x", 3)] ++ [MkAp] ++ [Slide 4, Unwind])
=> ("S", 3, compileC (EVar "f") [("f", 0), ("g", 1), ("x", 2)] ++ compileC (EVar "x") [("f", 1), ("g", 2), ("x", 3)] ++ [MkAp] ++ compileC (EAp (EVar "g") (EVar "x")) [("f", 1), ("g", 2), ("x", 3)] ++ [MkAp] ++ [Slide 4, Unwind])
=> ("S", 3, [Push 0] ++ compileC (EVar "x") [("f", 1), ("g", 2), ("x", 3)] ++ [MkAp] ++ compileC (EAp (EVar "g") (EVar "x")) [("f", 1), ("g", 2), ("x", 3)] ++ [MkAp] ++ [Slide 4, Unwind])
=> ("S", 3, [Push 0] ++ [Push 3] ++ [MkAp] ++ compileC (EAp (EVar "g") (EVar "x")) [("f", 1), ("g", 2), ("x", 3)] ++ [MkAp] ++ [Slide 4, Unwind])
=> ("S", 3, [Push 0] ++ [Push 3] ++ [MkAp] ++ compileC (EVar "g") [("f", 1), ("g", 2), ("x", 3)] ++ compileC (EVar "x") [("f", 2), ("g", 3), ("x", 4)] ++ [MkAp] ++ [MkAp] ++ [Slide 4, Unwind])
=> ("S", 3, [Push 0] ++ [Push 3] ++ [MkAp] ++ [Push 2] ++ compileC (EVar "x") [("f", 2), ("g", 3), ("x", 4)] ++ [MkAp] ++ [MkAp] ++ [Slide 4, Unwind])
=> ("S", 3, [Push 0] ++ [Push 3] ++ [MkAp] ++ [Push 2] ++ [Push 4] ++ [MkAp] ++ [MkAp] ++ [Slide 4, Unwind])
=> ("S", 3, [Push 0, Push 3, MkAp, Push 2, Push 4, MkAp, MkAp, Slide 4, Unwind])
```
