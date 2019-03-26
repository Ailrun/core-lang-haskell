## Rule for UpdateInt
| output | code            | stack             | dump | vStack | heap           | globals |
|--------|-----------------|-------------------|------|--------|----------------|---------|
| o      | UpdateInt n : i | a0 : ... : an : s | d    | n : v  | h              | m       |
| o      | i               | a0 : ... : an : s | d    | v      | h[an : NNum n] | m       |

## Rule for UpdateBool
| output | code             | stack             | dump | vStack | heap                 | globals |
|--------|------------------|-------------------|------|--------|----------------------|---------|
| o      | UpdateBool n : i | a0 : ... : an : s | d    | t : v  | h                    | m       |
| o      | i                | a0 : ... : an : s | d    | v      | h[an : NConstr t []] | m       |
