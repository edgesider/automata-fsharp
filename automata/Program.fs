module automata.main

open DFA
open NFA
open sre

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
        NFA.ofSeq (Set [ 0; 1; 2; 3 ]) (Set [ '0'; '1' ]) 0 (Set [ 3 ])
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

    let dfa0 = nfa0.toDFA ()
    //    printfn "%A" dfa0
    assert (dfa0.run "11" = true)
    assert (dfa0.run "10010111" = true)
    assert (dfa0.run "0100110" = true)
    assert (dfa0.run "01" = false)
    assert (dfa0.run "10" = false)
    assert (dfa0.run "010011" = false)
    assert (dfa0.run "110010" = false)

    // test NotChar
    let nfa1 =
        NFA.ofSeq (Set [ 0; 1; 2 ]) (Set [ '0'; '1'; '2'; '3' ]) 0 (Set [ 2 ])
            [ (0, Char '0', Set [ 1 ])
              (1, NotIn(Set [ '0' ]), Set [ 2 ]) ]
    assert (nfa1.run "00" = false)
    assert (nfa1.run "01" = true)
    assert (nfa1.run "02" = true)
    assert (nfa1.run "03" = true)

    let dfa11 = (nfa1.toDFA ()).renameStates(Seq.initInfinite id)
    assert (dfa11.run "00" = false)
    assert (dfa11.run "01" = true)
    assert (dfa11.run "02" = true)
    assert (dfa11.run "03" = true)

let testSRE () =
    // 识别0
    let m_0 = sre.Is '0'
    // 识别1
    let m_1 = sre.Is '1'
    // 识别2
    let m_2 = sre.Is '2'

    assert (m_0.run "0" = true)
    assert (m_1.run "1" = true)
    assert (m_2.run "2" = true)

    // +连接，/或，!*（前缀）克林闭包
    // 0(10)*2
    let m = m_0 + !*(m_1 / m_0) + m_2
    assert (m.run "01101010102" = true)

[<EntryPoint>]
let main argv =
    testDFA ()
    testNFA ()
    testSRE ()
    0
