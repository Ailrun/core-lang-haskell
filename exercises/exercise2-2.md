Invariant of the machine is

> `N * M = N * m + d + t`

1. This invariant is true for initial state because
   > `N * M = N * M + 0 + 0`
1. If this invariant is true for a specific state, then it is also true for its successor state, because
   > `N * M = N * m + d + t = N * m + (d - 1) + (t + 1)`
   is true for the first rule and
   > `N * M = N * m + 0 + t = N * (m - 1) + N + t`
   is true for the second rule.
1. If this invariant is true, then in a final state, `t = N * M` is true, because
   > `N * M = N * m + d + t = N * 0 + 0 + t = t`
