module automata.DFA

open automata
open utils

(*
Q, 状态集合
F, 转移函数
S, 开始状态
E, 终止状态
*)
type DFA<'state, 'char when 'state: comparison and 'char: comparison> =
    { Q: Set<'state>
      F: Map<'state, Map<'char, 'state>>
      S: 'state
      E: Set<'state> }

    member m.run(input: seq<'char>): bool =
        input
        |> Seq.scan (fun curr_q char ->
            match curr_q with
            | None -> None
            | Some q -> m.transmit char q) (Some m.S)
        |> Seq.takeWhile (fun curr_q -> curr_q.IsSome)
        |> Seq.last
        |> fun q ->
            match q with
            | None -> false
            | Some q -> m.E.Contains q

    member m.renameStates<'new_state when 'new_state: comparison>(state_seq: seq<'new_state>): DFA<'new_state, 'char> =
        let Q = Seq.take m.Q.Count state_seq |> Set.ofSeq

        let stateMap =
            (m.Q, Q)
            ||> Seq.zip
            |> Map.ofSeq

        let S = stateMap.[m.S]
        let E = Set.map (fun e -> stateMap.[e]) m.E

        DFA.ofSeq Q S E
            (m.F
             |> mapToSeq
             |> Seq.map (fun (q0, c, q1) -> (stateMap.[q0], c, stateMap.[q1])))

    (*返回值：
        None: 该状态下没有转移，或没有对应该字符的转移，即该转移不存在
        Some('state): 转移之后的状态*)
    member private m.transmit (char: 'char) (curr_q: 'state): Option<'state> =
        assert m.Q.Contains curr_q
        match m.F.TryFind curr_q with
        | None -> None
        | Some t -> t.TryFind char

    static member ofSeq<'state, 'char when 'state: comparison and 'char: comparison> (Q: Set<'state>) (S: 'state)
                  (E: Set<'state>) (ts: seq<'state * 'char * 'state>): DFA<'state, 'char> =
        { DFA.Q = Q
          F = seqToMap ts
          S = S
          E = E }
