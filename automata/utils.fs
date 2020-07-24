module automata.utils

open System

let addTransmit<'in_state, 'char, 'out_state when 'in_state: comparison and 'char: comparison and 'out_state: comparison>
    (map: Map<'in_state, Map<'char, 'out_state>>)
    (t: 'in_state * 'char * 'out_state)
    =
    let (curr, char, next) = t
    if not (map.ContainsKey curr)
    then map.Add(curr, (Map [ (char, next) ]))
    else map.Add(curr, map.[curr].Add(char, next))

let seqToMap<'in_state, 'char, 'out_state when 'in_state: comparison and 'char: comparison and 'out_state: comparison> (ts: seq<'in_state * 'char * 'out_state>): Map<'in_state, Map<'char, 'out_state>>
    = Seq.fold addTransmit Map.empty ts

let mapToSeq<'in_state, 'char, 'out_state when 'in_state: comparison and 'char: comparison and 'out_state: comparison> (map: Map<'in_state, Map<'char, 'out_state>>): seq<'in_state * 'char * 'out_state>
    =
    Map.fold (fun l q t -> l @ Map.fold (fun l c q1 -> (q, c, q1) :: l) [] t) [] map |> Seq.ofList

let TODO () = raise (Exception())

module Map =
    let keys (m: Map<'Key, 'T>) = Map.fold (fun keys k _ -> Set.add k keys) Set.empty m
