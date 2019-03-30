# Rule for Take
| Instructions | Frame Pointer | Stack             | Value Stack | Heap                                               | Code Store |
|--------------|---------------|-------------------|-------------|----------------------------------------------------|------------|
| Take t n : i | f             | c1 : ... : cn : s | v           | h                                                  | c          |
| i            | f'            | s                 | v           | h[f' : <c1, ..., cn, ([], null), ..., ([], null)>] | c          |

where `f'` repeats `([], null)` `t - n` times.

# Rule 1 for Move

| Instructions       | Frame Pointer | Stack | Value Stack | Heap                                                           | Code Store |
|--------------------|---------------|-------|-------------|----------------------------------------------------------------|------------|
| Move n (Arg k) : i | f             | s     | v           | h[f : <(i1, f1), ..., (ik, fk), ..., (in, fn), ..., (im, fm)>] | c          |
| i                  | f             | s     | v           | h[f : <(i1, f1), ..., (ik, fk), ..., (ik, fk), ..., (im, fm)>] | c          |

# Rule 2 for Move

| Instructions         | Frame Pointer | Stack | Value Stack | Heap                                            | Code Store |
|----------------------|---------------|-------|-------------|-------------------------------------------------|------------|
| Move n (Label l) : i | f             | s     | v           | h[f : <(i1, f1), ..., (in, fn), ..., (im, fm)>] | c[l : i']  |
| i                    | f             | s     | v           | h[f : <(i1, f1), ..., (i', f), ..., (im, fm)>]  | c          |

# Rule 3 for Move

| Instructions         | Frame Pointer | Stack | Value Stack | Heap                                                    | Code Store |
|----------------------|---------------|-------|-------------|---------------------------------------------------------|------------|
| Move n (Code i') : i | f             | s     | v           | h[f : <(i1, f1), ..., (in, fn), ..., (im, fm)>] | c          |
| i                    | f             | s     | v           | h[f : <(i1, f1), ..., (i', f), ..., (im, fm)>]          | c          |

# Rule 4 for Move

| Instructions            | Frame Pointer | Stack | Value Stack | Heap                                                | Code Store |
|-------------------------|---------------|-------|-------------|-----------------------------------------------------|------------|
| Move n (IntConst n) : i | f             | s     | v           | h[f : <(i1, f1), ..., (in, fn), ..., (im, fm)>]     | c          |
| i                       | f             | s     | v           | h[f : <(i1, f1), ..., (intCode, n), ..., (im, fm)>] | c          |
