module automata.sre

open NFA

type sre =
    static member Is<'char when 'char: comparison>(c: 'char) =
        NFA.ofSeq (Set [ 0; 1 ]) (Set [ c ]) 0 (Set [ 1 ]) [ (0, Char c, Set [ 1 ]) ]
    static member Not<'char when 'char: comparison>(c: 'char) =
        NFA.ofSeq (Set [ 0; 1 ]) (Set [ c ]) 0 (Set [ 1 ]) [ (0, NotIn(Set [ c ]), Set [ 1 ]) ]
    static member In<'char when 'char: comparison>(cs: Set<'char>) =
        NFA.ofSeq (Set [ 0; 1 ]) cs 0 (Set [ 1 ]) (Set.map (fun c -> (0, Char c, Set [ 1 ])) cs)
    static member NotIn<'char when 'char: comparison>(c: Set<'char>) =
        NFA.ofSeq (Set [ 0; 1 ]) c 0 (Set [ 1 ]) [ (0, NotIn c, Set [ 1 ]) ]
