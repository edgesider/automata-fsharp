module automata.sre

open NFA
open constructor

type sre<'char when 'char: comparison> =
    static member Is(c: 'char) =
        NFA.ofSeq (Set [ 0; 1 ]) (Set [ c ]) 0 (Set [ 1 ]) [ (0, Char c, Set [ 1 ]) ]

    static member Not(c: 'char) =
        NFA.ofSeq (Set [ 0; 1 ]) (Set [ c ]) 0 (Set [ 1 ]) [ (0, NotIn(Set [ c ]), Set [ 1 ]) ]

    static member In(cs: 'char seq) =
        let cs = cs |> Set.ofSeq
        NFA.ofSeq (Set [ 0; 1 ]) cs 0 (Set [ 1 ]) (Set.map (fun c -> (0, Char c, Set [ 1 ])) cs)

    static member NotIn(c: 'char seq) =
        let c = c |> Set.ofSeq
        NFA.ofSeq (Set [ 0; 1 ]) c 0 (Set [ 1 ]) [ (0, NotIn c, Set [ 1 ]) ]

let (+) m m1 = NFAConstructor.concatenate m m1
let (/) m m1 = NFAConstructor.alternate m m1
let (!*) m = NFAConstructor.kleene_closure m
