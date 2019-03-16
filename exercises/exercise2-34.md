## Rule 1
| forward | backward | heap                             |
|---------|----------|----------------------------------|
| f       | b        | h[f : NData t []]                |
| f       | b        | h[f : NMarked Done (NData t [])] |

## Rule 2
| forward | backward | heap                                     |
|---------|----------|------------------------------------------|
| f       | b        | h[f : NData t a:as]                      |
| a       | f        | h[f : NMarked (Visits 1) (NData t b:as)] |

## Rule 3
| forward         | backward | heap                                                                                                                                                                 |
|-----------------|----------|----------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| f               | b        | h[<br/>&nbsp;&nbsp;f : NMarked Done n<br/>&nbsp;&nbsp;b : NMarked (Visits v) (NData t [a<sub>1</sub>,...,a<sub>v-1</sub>,b',a<sub>v+1</sub>,...a<sub>m</sub>])<br/>] |
| a<sub>v+1</sub> | b        | h[b : NMarked (Visits (v + 1)) (NData t [a<sub>1</sub>,...,a<sub>v-1</sub>,f,b',...a<sub>m</sub>])<br/>])]                                                           |

if v < m

## Rule 4
| forward | backward | heap                                                                                                                                |
|---------|----------|-------------------------------------------------------------------------------------------------------------------------------------|
| f       | b        | h[<br/>&nbsp;&nbsp;f : NMarked Done n<br/>&nbsp;&nbsp;b : NMarked (Visits v) (NData t [a<sub>1</sub>,...,a<sub>v-1</sub>,b'])<br/>] |
| b       | b'       | h[b : NMarked Done (NData t [a<sub>1</sub>,...,a<sub>v-1</sub>,f])<br/>])]                                                          |

if v = m
