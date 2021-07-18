let rec printStr = (ast: Reader.ast): string => {
  switch ast {
  | {data: MalList(x)} =>
    "(" ++
    x->Js.Array2.map(printStr)->Js.Array2.filter(y => y !== "")->Js.Array2.joinWith(" ") ++ ")"
  | {data: MalString(x)} =>
    "\"" ++
    {
      open Js.String2
      x
      ->replaceByRe(%re("/\\\\/g"), "\\\\")
      ->replaceByRe(%re("/\"/g"), "\\\"")
      ->replaceByRe(%re("/\\n/g"), "\\n")
    } ++ "\""
  | {data: MalInt(x)} => x->string_of_int
  | {data: MalFloat(x)} => x->Js.Float.toString
  | {data: MalTrue} => "true"
  | {data: MalFalse} => "false"
  | {data: MalNil} => "nil"
  | {data: MalSpliceUnquote} => "~@"
  | {data: MalAtom(x)} => x
  }
}
