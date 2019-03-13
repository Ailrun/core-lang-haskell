## Rule 1 - Evaluation Rule
| stack            | dump | heap                                                                                                        | globals |
|------------------|------|-------------------------------------------------------------------------------------------------------------|---------|
| a : a1 : a2 : [] | d    | h[<br/>  a : NPrim Add<br/>  a1 : NAp a b1<br/>  a2 : NAp a1 b2<br/>  b1 : NNum n1<br/>  b2 : NNum n2<br/>] | f       |
| a2 : []          | d    | h[a2 : NNum (n1 + n2)]                                                                                      | f       |

## Rule 2 - If Rule 1 Is Not Applicable
| stack            | dump               | heap                                                                                     | globals |
|------------------|--------------------|------------------------------------------------------------------------------------------|---------|
| a : a1 : a2 : [] | d                  | h[<br/>  a : NPrim Add<br/>  a1 : NAp a b1<br/>  a2 : NAp a1 b2<br/>  b2 : NNum n2<br/>] | f       |
| b1 : []          | (a1 : a2 : []) : d | h                                                                                        | f       |

## Rule 3 - If Rule 1 & Rule 2 Are Not Applicable
| stack            | dump          | heap                                                                  | globals |
|------------------|---------------|-----------------------------------------------------------------------|---------|
| a : a1 : a2 : [] | d             | h[<br/>  a : NPrim Add<br/>  a1 : NAp a b1<br/>  a2 : NAp a1 b2<br/>] | f       |
| b2 : []          | (a2 : []) : d | h                                                                     | f       |
