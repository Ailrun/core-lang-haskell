## Rule 1 - Evaluation Rule
| stack            | dump | heap                                                                                                                                            | globals |
|------------------|------|-------------------------------------------------------------------------------------------------------------------------------------------------|---------|
| a : a1 : a2 : [] | d    | h[<br/>&nbsp;&nbsp;a : NPrim NCasePair<br/>&nbsp;&nbsp;a1 : NAp a b1<br/>&nbsp;&nbsp;a2 : NAp a1 b2<br/>&nbsp;&nbsp;b1 : NData 1 [c1, c2]<br/>] | f       |
| a2 : []          | d    | h[<br/>&nbsp;&nbsp;a2 : NAp a3 c2<br/>&nbsp;&nbsp;a3 : NAp b2 c1<br/>]                                                                          | f       |

## Rule 2 - If Rule 1 Is Not Applicable
| stack            | dump               | heap                                                                                                      | globals |
|------------------|--------------------|-----------------------------------------------------------------------------------------------------------|---------|
| a : a1 : a2 : [] | d                  | h[<br/>&nbsp;&nbsp;a : NPrim NCasePair<br/>&nbsp;&nbsp;a1 : NAp a b1<br/>&nbsp;&nbsp;a2 : NAp a1 b2<br/>] | f       |
| b1 : []          | (a1 : a2 : []) : d | h                                                                                                         | f       |
