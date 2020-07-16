module automata.main

open automata.DFA
open automata.NFA

let testDFA () =
    // 0*1*
    // build from map
    let dfa0 =
        { DFA.Q = Set [ 0; 1 ]
          F =
              Map
                  [ (0,
                     Map
                         [ ('0', 0)
                           ('1', 1) ])
                    (1, Map [ ('1', 1) ]) ]
          S = 0
          E = Set [ 1 ] }

    // tuple as state
    let dfa1 =
        DFA.ofSeq
            (Set
                [ (0, 0)
                  (0, 1) ]) (0, 0) (Set [ (0, 1) ])
            [ ((0, 0), '0', (0, 0))
              ((0, 0), '1', (0, 1))
              ((0, 1), '1', (0, 1)) ]

    // int as char
    let dfa2 =
        DFA.ofSeq (Set [ 0; 1 ]) 0 (Set [ 1 ])
            [ (0, 0, 0)
              (0, 1, 1)
              (1, 1, 1) ]


    // 0*1*
    assert (dfa0.run "000111" = true)
    assert (dfa1.run "0000111" = true)
    assert (dfa2.run [ 0; 0; 0; 1; 1; 1 ] = true)

let testNFA () =
    // 识别以相同字符开始和结尾的字符串
    // 0(0|1)*0 | 1(0|1)*1
    let nfa0 =
        NFA.ofSeq (Set [ 0; 1; 2; 3 ]) 0 (Set [ 3 ])
            [ (0, Char '0', Set [ 1 ])
              (0, Char '1', Set [ 2 ])
              (1, Char '0', Set [ 1; 3 ])
              (1, Char '1', Set [ 1 ])
              (2, Char '0', Set [ 2 ])
              (2, Char '1', Set [ 2; 3 ]) ]

    assert (nfa0.run "11" = true)
    assert (nfa0.run "10010111" = true)
    assert (nfa0.run "0100110" = true)
    assert (nfa0.run "01" = false)
    assert (nfa0.run "10" = false)
    assert (nfa0.run "010011" = false)
    assert (nfa0.run "110010" = false)

[<EntryPoint>]
let main argv =
    testDFA ()
    testNFA ()
    0
