module automata.utils

let seqToMap<'in_state, 'char, 'out_state when 'in_state: comparison and 'char: comparison and 'out_state: comparison> (ts: seq<'in_state * 'char * 'out_state>): Map<'in_state, Map<'char, 'out_state>>
    =
    Seq.fold (fun map t ->
        let (curr, char, next) = t
        if not (map.ContainsKey curr)
        then map.Add(curr, (Map [ (char, next) ]))
        else map.Add(curr, map.[curr].Add(char, next))) Map.empty ts
