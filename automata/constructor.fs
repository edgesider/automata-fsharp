module automata.constructor

open NFA
open utils

type NFAConstructor =

    static member concatenate (m: NFA<int, 'char>) (m1: NFA<int, 'char>): NFA<int, 'char> =
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
          E = m1.E
          R = Set.empty }

    static member alternate (m: NFA<int, 'char>) (m1: NFA<int, 'char>): NFA<int, 'char> =
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
          E = Set [ newEnd ]
          R = Set.empty }

    static member kleene_closure(m: NFA<int, 'char>): NFA<int, 'char> =
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
          E = Set [ newEnd ]
          R = Set.empty }

    static member look_forward (m: NFA<int, 'char>) (to_look: NFA<int, 'char>) = ()

    static member look_backward (m: NFA<int, 'char>) (to_look: NFA<int, 'char>) = ()
