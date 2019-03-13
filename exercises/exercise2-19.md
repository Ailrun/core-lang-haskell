## Rule 1 - Evaluation Rule For False
| stack                 | dump | heap                                                                                                                                                               | globals |
|-----------------------|------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------|---------|
| a : a1 : a2 : a3 : [] | d    | h[<br/>&nbsp;&nbsp;a : NPrim NIf<br/>&nbsp;&nbsp;a1 : NAp a b1<br/>&nbsp;&nbsp;a2 : NAp a1 b2<br/>&nbsp;&nbsp;a3 : NAp a2 b3<br/>&nbsp;&nbsp;b1 : NData 1 []<br/>] | f       |
| a3 : []               | d    | h[a3 : NInd b3]                                                                                                                                                    | f       |

## Rule 2 - Evaluation Rule For True
| stack                 | dump | heap                                                                                                                                                               | globals |
|-----------------------|------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------|---------|
| a : a1 : a2 : a3 : [] | d    | h[<br/>&nbsp;&nbsp;a : NPrim NIf<br/>&nbsp;&nbsp;a1 : NAp a b1<br/>&nbsp;&nbsp;a2 : NAp a1 b2<br/>&nbsp;&nbsp;a3 : NAp a2 b3<br/>&nbsp;&nbsp;b1 : NData 2 []<br/>] | f       |
| a3 : []               | d    | h[a3 : NInd b2]                                                                                                                                                    | f       |

## Rule 3 - If Rule 1 & Rule 2 Are Not Applicable
| stack                 | dump                    | heap                                                                                                                               | globals |
|-----------------------|-------------------------|------------------------------------------------------------------------------------------------------------------------------------|---------|
| a : a1 : a2 : a3 : [] | d                       | h[<br/>&nbsp;&nbsp;a : NPrim NIf<br/>&nbsp;&nbsp;a1 : NAp a b1<br/>&nbsp;&nbsp;a2 : NAp a1 b2<br/>&nbsp;&nbsp;a3 : NAp a2 b3<br/>] | f       |
| b1 : []               | (a1 : a2 : a3 : []) : d | h                                                                                                                                  | f       |
