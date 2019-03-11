In following example,

```
f x y = case x of
        <1> -> case y of
                <1> -> 1;
        <2> -> 2
```

`<2>` will be a part of `case y of ...` with the current parser.
