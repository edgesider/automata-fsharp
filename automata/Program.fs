module automata.main

open automata.DFA

let testDFA () =
    let dfa0 =
        { DFA.F =
              fun state char ->
                  match state with
                  | 0 ->
                      match char with
                      | '0' -> Some(0)
                      | '1' -> Some(1)
                      | _ -> None
                  | 1 ->
                      match char with
                      | '1' -> Some(1)
                      | _ -> None
          S = 0
          E = 1 }

    let dfa1 =
        { DFA.F =
              fun state char ->
                  match state with
                  | 0 ->
                      match char with
                      | 0 -> Some(0)
                      | 1 -> Some(1)
                      | _ -> None
                  | 1 ->
                      match char with
                      | 1 -> Some(1)
                      | _ -> None
          S = 0
          E = 1 }


    let dfa2 =
        DFA.ofMap
            (Map
                [ (0,
                   Map
                       [ ('0', 0)
                         ('1', 1) ])
                  (1, Map [ ('1', 1) ]) ]) 0 1

    let dfa3 =
        DFA.ofSeq
            [ (0, '0', 0)
              (0, '1', 1)
              (1, '1', 1) ] 0 1

    let dfa4 =
        DFA.ofSeq
            [ (0, 0, 0)
              (0, 1, 1)
              (1, 1, 1) ] 0 1


    dfa0.run "000111" |> printfn "%A"
    dfa1.run [ 0; 0; 0; 1; 1; 1 ] |> printfn "%A"
    dfa2.run "000111" |> printfn "%A"
    dfa3.run "0000111" |> printfn "%A"
    dfa4.run [ 0; 0; 0; 1; 1; 1 ] |> printfn "%A"

[<EntryPoint>]
let main argv =
    testDFA ()
    0
