module automata.NFA

open automata
open utils

type EChar<'char> =
    | Char of 'char
    | Epsilon of unit

type NFA<'state, 'char when 'state: comparison and 'char: comparison> =
    { Q: Set<'state>
      F: Map<'state, Map<EChar<'char>, Set<'state>>>
      S: 'state
      E: Set<'state> }

    member m.run(input: seq<'char>) =
        input
        |> Seq.scan (fun curr_qs char ->
            curr_qs
            |> m.epsilon_closure
            |> m.transmit (Char char)
            |> m.epsilon_closure) (Set [ m.S ])
        |> Seq.takeWhile (fun qs -> not (Set.isEmpty qs))
        |> Seq.last
        |> Set.intersect m.E
        |> Set.isEmpty
        |> not

    (*返回值：
        None: 该状态下没有转移，或没有对应该字符的转移，即该转移不存在
        Some(Set<'state>): 转移之后的状态集合*)
    member private m.single_transmit (char: EChar<'char>) (q: 'state): Option<Set<'state>> =
        assert m.Q.Contains q
        match m.F.TryFind q with
        | None -> None // 该状态下没有转移
        | Some (t) -> t.TryFind char

    member private m.transmit (char: EChar<'char>) (qs: Set<'state>): Set<'state> =
        Set.fold (fun set q ->
            match m.single_transmit char q with
            | None -> set
            | Some (qs) -> Set.union set qs) Set.empty qs

    member private m.epsilon_closure(qs: Set<'state>): Set<'state> =
        let new_qs = m.transmit (Epsilon()) qs
        if new_qs.IsSubsetOf qs
        then qs
        else m.epsilon_closure (Set.union qs new_qs)

    static member ofSeq Q S E (ts: seq<'state * EChar<'char> * Set<'state>>): NFA<'state, 'char> =
        { NFA.Q = Q
          F = seqToMap ts
          S = S
          E = E }
