First, let's prove that `aEval (aCompile e ++ is, s) = aEval (is, aInterpret e : s)`

- For `e = Num n :: AExpr`,
    `aInterpret e = n` and `aEval (aCompile e ++ is, s) = aEval ([INum n] ++ is, s) = aEval (is, n : s)`
- For `e = Plus e1 e2` where `aEval (aCompile e1 ++ is, s) = aEval (is, aInterpret e1 : s) = aEval (is, n1 : s)` and `aEval (aCompile e2 ++ is, s) = aEval (is, aInterpret e2 : s) = aEval (is, n2 : s)`,
    ```
    aEval (aCompile e ++ is, s)
      = aEval (aCompile e1 ++ aCompile e2 ++ [IPlus] ++ is, s)
      = aEval(aCompile e2 ++ [IPlus] ++ is, n1 : s)
      = aEval([IPlus] ++ is, n2 : n1 : s)
      = aEval(is, n1 + n2 : s)
      = aEval(is, aInterpret e1 + aInterpret e2 : s)
      = aEval(is, aInterpret e : s)
    ```
- For `e = Mult e1 e2` where `aEval (aCompile e1 ++ is, s) = aEval (is, aInterpret e1 : s) = aEval (is, n1 : s)` and `aEval (aCompile e2 ++ is, s) = aEval (is, aInterpret e2 : s) = aEval (is, n2 : s)`,
    ```
    aEval (aCompile e ++ is, s)
      = aEval (aCompile e1 ++ aCompile e2 ++ [IMult] ++ is, s)
      = aEval(aCompile e2 ++ [IMult] ++ is, n1 : s)
      = aEval([IMult] ++ is, n2 : n1 : s)
      = aEval(is, n1 \* n2 : s)
      = aEval(is, aInterpret e1 \* aInterpret e2 : s)
      = aEval(is, aInterpret e : s)
    ```

Now, for `is = []` and `s = []`, `aInterpret e = aEval ([], [aInterpret e]) = aEval (aCompile e, [])`
