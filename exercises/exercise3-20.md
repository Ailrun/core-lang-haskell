One could define `Y` combinator like following.

```
Y f = f (Y f)
```

After compilation, it becomes

```
[("Y", 1, [Push 0, PushGlobal "Y", MkAp, Push 1, MkAp, Update 1, Pop 1, Unwind])]
```

However, with this definition, the machine need to spend more steps since it always re-instantiate `Y` combinator when its argument is evaluated.
