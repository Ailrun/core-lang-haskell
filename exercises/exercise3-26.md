If there is nothing like following code,

```
      case getDump state of
        (c, as') : dump' -> putDump dump' (putCode c (putStack (a : as') state))
        _ -> state

```

one should update `initialCode` since **Unwind** cannot handle empty dump.

However, with the code, **Unwind** works well with `main` and you don't need to update `initialCode`.
