Mark 1 G machine spent 63 steps to terminate the program, where Mark 2 G machine spent 81 steps.
This is a fair comparison, because their steps have similar complexity with each other.

Mark 2 G machine spent less steps when tested with a program that has repeated works. For example, with following program, Mark 2 spent 125 steps where Mark 1 spent 135 steps.

```
twice f x = f (f x);
id x = x;
main = twice twice (id (id (id (id id)))) 3
```
