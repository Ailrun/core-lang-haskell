```
main = twicePar (twicePar (twicePar (S K K))) 3;
twicePar f x = par f (f x)
```

does not work with Mark 2 implementation.
That is because when a node is unlocked, only single task among blocked tasks can be resumed, and the resumed can be depends on a blocked task (which should be resumed).
