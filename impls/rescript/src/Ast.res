type rec t = {loc: int, data: malType}
and hash = {
  key: t,
  value: t,
}
and malType =
  | MalList(array<t>)
  | MalVector(array<t>)
  | MalHashMap(array<hash>)
  | MalQuote(t)
  | MalQuasiQuote(t)
  | MalUnquote(t)
  | MalSpliceUnquote(t)
  | MalDeref(t)
  | MalWithMeta(t, t)
  | MalString(string)
  | MalInt(int)
  | MalFloat(float)
  | MalTrue
  | MalFalse
  | MalNil
  | MalKeyword(string)
  | MalSymbol(string)
