module automata.NFA

open System
open automata
open DFA
open utils

type EChar<'char when 'char: comparison> =
    | Char of 'char
    | NotIn of Set<'char>
    | Epsilon

type F<'state, 'char when 'state: comparison and 'char: comparison> = Map<'state, Map<EChar<'char>, Set<'state>>>

type NFA<'state, 'char when 'state: comparison and 'char: comparison> =
    { Q: Set<'state>
      C: Set<'char>
      F: F<'state, 'char>
      S: 'state
      E: Set<'state> }

type EChar<'char when 'char: comparison> with
    member x.isAccept<'state when 'state: comparison> (char: EChar<'char>) (_m: NFA<'state, 'char>): bool =
        match x with
        | Epsilon -> char = x
        | Char _ -> char = x
        | NotIn cs ->
            match char with
            | Char char -> cs.Contains char |> not
            | _ -> false

type NFA<'state, 'char when 'state: comparison and 'char: comparison> with

    member m.run(input: seq<'char>) =
        input
        // 退出的原因有两种：input结束或者状态集为空。如果使用单独的curr_qs，就无法得知takeWhile退出的原因了。
        |> Seq.scan (fun (_prev_qs, curr_qs) char -> (curr_qs, m.epsilonTransmit char curr_qs))
               (Set [ m.S ], Set [ m.S ])
        |> Seq.takeWhile (fun (prev, _) -> Set.isEmpty prev |> not)
        |> Seq.last
        |> snd
        |> m.isEnd

    member private m.singleTransmit (char: EChar<'char>) (q: 'state): Set<'state> =
        assert m.Q.Contains q
        match m.F.TryFind q with
        | None -> Set.empty // 该状态下没有转移
        | Some t ->
            // 遍历每条路径
            t
            |> Map.fold (fun qs c to_qs ->
                // 如果该路径接受该字符，将转移后的状态加入qs中
                if c.isAccept char m then Set.union qs to_qs else qs) Set.empty

    member private m.transmit (char: EChar<'char>) (qs: Set<'state>): Set<'state> =
        Set.fold (fun set q -> Set.union set (m.singleTransmit char q)) Set.empty qs

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

    member m.unitEnds(): NFA<int, 'char> =
        if (m.E.Count <= 1) then
            m.renameStates (Seq.initInfinite id)
        else
            let m = m.renameStates (Seq.initInfinite id)
            let newEnd = Set [ (m.Q |> Seq.max) + 1 ]
            let f = Set.fold (fun f e -> addTransmit f (e, Epsilon, newEnd)) m.F m.E
            { m with
                  Q = m.Q + newEnd
                  F = f
                  E = newEnd }

    member m.toDFA(): DFA<int, 'char> =
        // 从processingQS中取出一个状态，为该状态遍历所有字符，得到新的状态和相应的转移函数，递归调用。
        // TODO 不需要遍历所有的字符，可以通过F表来判断哪些字符可以转移，这样就能处理NotIn之类的特殊字符
        let rec f
                (dfaQ: Set<Set<'state>>)
                (dfaF: Set<Set<'state> * 'char * Set<'state>>)
                (processingQS: List<Set<'state>>) =
            match processingQS with
            | head :: tail ->
                Seq.fold (fun (qs, fs) c ->
                    let q = m.epsilonTransmit c head

                    let qs =
                        // 如果是新的状态，则加入要处理的状态集
                        if (dfaQ.Contains q || List.contains q processingQS)
                        then qs
                        else q :: qs
                    (qs, Set.add (head, c, q) fs)) ([], Set.empty) m.C
                |> fun (newQS, newFS) -> (dfaQ.Add head, newFS + dfaF, newQS @ tail)
                |||> f
            | [] -> (dfaQ, dfaF)

        let (Q, F) = f Set.empty Set.empty [ Set [ m.S ] ]
        let E = Set.filter (fun q -> m.isEnd q) Q
        (DFA.ofSeq Q (Set [ m.S ]) E F).renameStates(Seq.initInfinite id)

    member m.renameStates<'new_state when 'new_state: comparison>(state_seq: seq<'new_state>): NFA<'new_state, 'char> =
        let Q = Seq.take m.Q.Count state_seq |> Set.ofSeq
        if Set.count Q <> m.Q.Count then raise (ArgumentException("repeated state"))

        let state_map =
            (m.Q, Q)
            ||> Seq.zip
            |> Map.ofSeq

        let S = state_map.[m.S]
        let E = Set.map (fun e -> state_map.[e]) m.E

        NFA.ofSeq Q m.C S E
            (m.F
             |> mapToSeq
             |> Seq.map (fun (q0, c, q1) -> (state_map.[q0], c, Set.map (fun q -> state_map.[q]) q1)))

    static member ofSeq<'state, 'char when 'state: comparison and 'char: comparison> (Q: Set<'state>) (C: Set<'char>)
                  (S: 'state) (E: Set<'state>) (ts: seq<'state * EChar<'char> * Set<'state>>): NFA<'state, 'char> =
        { NFA.Q = Q
          C = C
          F = seqToMap ts
          S = S
          E = E }


    // SRE
    static member (+)(m: NFA<int, 'char>, m1: NFA<int, 'char>): NFA<int, 'char> =
        let m = m.unitEnds ()
        let m1 = m1.unitEnds ()
        let m1 = m1.renameStates (Seq.initInfinite (fun i -> i + m.Q.Count))
        { NFA.Q = m.Q + m1.Q
          C = m.C + m1.C
          F =
              ((m.F |> Map.toSeq), (m1.F |> Map.toSeq))
              ||> Seq.append
              |> Map.ofSeq
              |> fun map -> addTransmit map (m.E |> Seq.head, Epsilon, Set [ m1.S ])
          S = m.S
          E = m1.E }

    static member (/)(m: NFA<int, 'char>, m1: NFA<int, 'char>): NFA<int, 'char> =
        let m = m.unitEnds ()
        let m1 = m1.unitEnds ()
        let m1 = m1.renameStates (Seq.initInfinite (fun i -> i + m.Q.Count))
        let newStart = m.Q.Count + m1.Q.Count
        let newEnd = m.Q.Count + m1.Q.Count + 1

        let F =
            ((m.F |> Map.toSeq), (m1.F |> Map.toSeq))
            ||> Seq.append
            |> Map.ofSeq
            |> fun map -> addTransmit map (newStart, Epsilon, Set [ m.S; m1.S ])
            |> fun map -> addTransmit map (m.E |> Seq.head, Epsilon, Set [ newEnd ])
            |> fun map -> addTransmit map (m1.E |> Seq.head, Epsilon, Set [ newEnd ])
        { NFA.Q = m.Q + m1.Q + Set [ newStart; newEnd ]
          C = m.C + m1.C
          F = F
          S = newStart
          E = Set [ newEnd ] }

    static member (!*)(m: NFA<int, 'char>): NFA<int, 'char> =
        let m = m.unitEnds ()
        let newStart = m.Q.Count
        let newEnd = m.Q.Count + 1

        let F =
            m.F
            |> fun map -> addTransmit map (m.E |> Seq.head, Epsilon, Set [ m.S; newEnd ])
            |> fun map -> addTransmit map (newStart, Epsilon, Set [ m.S; newEnd ])
        { NFA.Q = m.Q + Set [ newStart; newEnd ]
          C = m.C
          F = F
          S = newStart
          E = Set [ newEnd ] }
