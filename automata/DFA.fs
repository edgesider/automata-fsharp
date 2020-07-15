module automata.DFA

type state = int

type Transmit<'a> = state -> 'a -> Option<state>

exception StateError of state: state with
    override x.Message = sprintf "no such state: %A" x.state

(*
F, 转移函数
S, 开始状态
E, 终止状态
*)
type DFA<'char when 'char: comparison> =
    { F: Transmit<'char>
      S: state
      E: state }

    member m.run(input: seq<'char>) =
        let input' = List.ofSeq input

        let rec run' (m: DFA<'char>) (input: List<'char>) (curr: state) =
            match input with
            | head :: tail ->
                match m.F curr head with
                | Some (next) -> run' m tail next
                | None -> None
            | [] -> Some(curr)

        match run' m input' m.S with
        | Some (s) -> s = m.E
        | None -> false

    static member ofMap<'a> (map: Map<state, Map<'char, state>>) S E: DFA<'char> =
        { F =
              fun state char ->
                  match map.TryFind(state) with
                  | Some (m) -> m.TryFind(char)
                  | None -> raise (StateError state)
          S = S
          E = E }

    static member ofSeq<'a> (ts: seq<state * 'char * state>) S E: DFA<'char> =
        DFA.ofMap
            (Seq.fold (fun m t ->
                let (curr, char, next) = t

                let m2 =
                    if not (m.ContainsKey curr) then m.Add(curr, Map.empty) else m
                m2.Add(curr, (m2.[curr].Add(char, next)))) Map.empty ts) S E
