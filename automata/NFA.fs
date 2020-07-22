module automata.NFA

open automata
open DFA
open utils

type EChar<'char> =
    | Char of 'char
    | Epsilon

type NFA<'state, 'char when 'state: comparison and 'char: comparison> =
    { Q: Set<'state>
      C: Set<'char>
      F: Map<'state, Map<EChar<'char>, Set<'state>>>
      S: 'state
      E: Set<'state> }

    member m.run(input: seq<'char>) =
        input
        |> Seq.scan (fun curr_qs char -> m.epsilonTransmit char curr_qs) (Set [ m.S ])
        |> Seq.takeWhile (fun qs -> not (Set.isEmpty qs))
        |> Seq.last
        |> m.isEnd

    member m.toDFA(): DFA<Set<'state>, 'char> =
        // 从processingQS中取出一个状态，为该状态遍历所有字符，得到新的状态和相应的转移函数，递归调用。
        let rec f
                (dfaQ: Set<Set<'state>>)
                (dfaF: Set<Set<'state> * 'char * Set<'state>>)
                (processingQS: List<Set<'state>>) =
            match processingQS with
            | head :: tail ->
                Seq.fold (fun (qs, fs) c ->
                    let q = m.epsilonTransmit c head

                    let qs =
                        if (dfaQ.Contains q || List.contains q processingQS)
                        then qs
                        else q :: qs
                    (qs, Set.add (head, c, q) fs)) ([], Set.empty) m.C
                |> fun (newQS, newFS) -> (dfaQ.Add head, newFS + dfaF, newQS @ tail)
                |||> f
            | [] -> (dfaQ, dfaF)

        let (Q, F) = f Set.empty Set.empty [ Set [ m.S ] ]
        let E = Set.filter (fun q -> m.isEnd q) Q
        DFA.ofSeq Q (Set [ m.S ]) E F

    member m.renameStates<'new_state when 'new_state: comparison>(state_seq: seq<'new_state>): NFA<'new_state, 'char> =
        let Q = Seq.take m.Q.Count state_seq |> Set.ofSeq

        let stateMap =
            (m.Q, Q)
            ||> Seq.zip
            |> Map.ofSeq

        let S = stateMap.[m.S]
        let E = Set.map (fun e -> stateMap.[e]) m.E

        NFA.ofSeq Q m.C S E
            (m.F
             |> mapToSeq
             |> Seq.map (fun (q0, c, q1) -> (stateMap.[q0], c, Set.map (fun q -> stateMap.[q]) q1)))

    (*返回值：
        None: 该状态下没有转移，或没有对应该字符的转移，即该转移不存在
        Some(Set<'state>): 转移之后的状态集合*)
    member private m.singleTransmit (char: EChar<'char>) (q: 'state): Option<Set<'state>> =
        assert m.Q.Contains q
        match m.F.TryFind q with
        | None -> None // 该状态下没有转移
        | Some t -> t.TryFind char

    member private m.transmit (char: EChar<'char>) (qs: Set<'state>): Set<'state> =
        Set.fold (fun set q ->
            match m.singleTransmit char q with
            | None -> set
            | Some qs -> Set.union set qs) Set.empty qs

    member private m.epsilonClosure(qs: Set<'state>): Set<'state> =
        let new_qs = m.transmit Epsilon qs
        if new_qs.IsSubsetOf qs then qs else m.epsilonClosure (Set.union qs new_qs)

    member private m.epsilonTransmit (char: 'char) (qs: Set<'state>) =
        qs
        |> m.epsilonClosure
        |> m.transmit (Char char)
        |> m.epsilonClosure

    member private m.isEnd(qs: Set<'state>) =
        qs
        |> Set.intersect m.E
        |> Set.isEmpty
        |> not

    static member ofSeq<'state, 'char when 'state: comparison and 'char: comparison> (Q: Set<'state>) (C: Set<'char>)
                  (S: 'state) (E: Set<'state>) (ts: seq<'state * EChar<'char> * Set<'state>>): NFA<'state, 'char> =
        { NFA.Q = Q
          C = C
          F = seqToMap ts
          S = S
          E = E }
