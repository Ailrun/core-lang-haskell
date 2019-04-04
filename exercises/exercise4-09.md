It cannot detect comparison mode in the following program,
and it is not able to run.

```
multipleof3 x = ((x / 3) * 3) == x
f y = if (multipleof3 y) 0 1
```
