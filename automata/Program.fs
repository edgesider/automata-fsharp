module automata.main

open DFA
open NFA
open sre

let assertTrue ex = assert (ex = true)
let assertFalse ex = assert (ex = false)

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
    dfa0.run "000111" |> assertTrue
    dfa1.run "0000111" |> assertTrue
    dfa2.run [ 0; 0; 0; 1; 1; 1 ] |> assertTrue

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

    nfa0.run "11" |> assertTrue
    nfa0.run "10010111" |> assertTrue
    nfa0.run "0100110" |> assertTrue
    nfa0.run "01" |> assertFalse
    nfa0.run "10" |> assertFalse
    nfa0.run "010011" |> assertFalse
    nfa0.run "110010" |> assertFalse

    let dfa0 = nfa0.toDFA ()
    //    printfn "%A" dfa0
    dfa0.run "11" |> assertTrue
    dfa0.run "10010111" |> assertTrue
    dfa0.run "0100110" |> assertTrue
    dfa0.run "01" |> assertFalse
    dfa0.run "10" |> assertFalse
    dfa0.run "010011" |> assertFalse
    dfa0.run "110010" |> assertFalse

    // test NotChar
    let nfa1 =
        NFA.ofSeq (Set [ 0; 1; 2 ]) (Set [ '0'; '1'; '2'; '3' ]) 0 (Set [ 2 ])
            [ (0, Char '0', Set [ 1 ])
              (1, NotIn(Set [ '0' ]), Set [ 2 ]) ]
    nfa1.run "00" |> assertFalse
    nfa1.run "01" |> assertTrue
    nfa1.run "02" |> assertTrue
    nfa1.run "03" |> assertTrue

    let dfa11 = (nfa1.toDFA ()).renameStates(Seq.initInfinite id)
    dfa11.run "00" |> assertFalse
    dfa11.run "01" |> assertTrue
    dfa11.run "02" |> assertTrue
    dfa11.run "03" |> assertTrue

    // 测试多个路径
    let m =
        NFA.ofSeq (Set [ 0; 1; 2; 3 ]) (Set [ '1'; '2'; '3' ]) 0 (Set [ 1; 3 ])
            // 1|[^2]3
            [ (0, Char '1', Set [ 1 ])
              (0, NotIn(Set [ '2' ]), Set [ 2 ])
              (2, Char '3', Set [ 3 ]) ]
    m.run "1" |> assertTrue
    m.run "13" |> assertTrue
    m.run "23" |> assertFalse

    // 测试空状态提前退出
    let m = sre.Is '1' + sre.Is '2' + sre.Is '3'

    let s =
        seq {
            for c in "1245" do
                // 不应该访问到'5'
                if c = '5' then assert false
                c
        }
    m.run s |> assertFalse

let testSRE () =
    // 识别0
    let m_0 = sre.Is '0'
    // 识别1
    let m_1 = sre.Is '1'
    // 识别2
    let m_2 = sre.Is '2'

    m_0.run "0" |> assertTrue
    m_1.run "1" |> assertTrue
    m_2.run "2" |> assertTrue

    // +连接，/或，!*（前缀）克林闭包
    // 0(10)*2
    let m = m_0 + !*(m_1 / m_0) + m_2
    m.run "01101010102" |> assertTrue

    // 0(123)*[^456]0
    let m = sre.Is '0' + !*(sre.Is '1' + sre.Is '2' + sre.Is '3') + sre.NotIn [ '4'; '5'; '6' ] + sre.Is '0'
    m.run "012312312320" |> assertTrue

[<EntryPoint>]
let main argv =
    testDFA ()
    testNFA ()
    testSRE ()
    0
