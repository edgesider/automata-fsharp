Automata（自动机）
===

用F#实现的DFA和NFA，以及一些特殊功能。

## 特殊功能

### SRE

使用F#的运算符重载来构建正则表达式，并直接转为NFA。

示例：

```f#
let m_0 = re.Is '0'
// 识别1
let m_1 = re.Is '1'
// 识别2
let m_2 = re.Is '2'

assert (m_0.run "0" = true)
assert (m_1.run "1" = true)
assert (m_2.run "2" = true)

// +连接，/或，!*（前缀）克林闭包
// 0(10)*2
let m = m_0 + !*(m_1 / m_0) + m_2
assert (m.run "01101010102" = true)
```
