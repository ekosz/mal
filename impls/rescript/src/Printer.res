let rec printStr = (ast: Reader.ast): string => {
  switch ast {
  | {data: MalList(x)} => "(" ++ x->Js.Array2.map(printStr)->Js.Array2.joinWith(" ") ++ ")"
  | {data: MalVector(x)} => "[" ++ x->Js.Array2.map(printStr)->Js.Array2.joinWith(" ") ++ "]"
  | {data: MalHashMap(x)} =>
    "{" ++
    x
    ->Js.Array2.map(({key, value}) => {
      printStr(key) ++ " " ++ printStr(value)
    })
    ->Js.Array2.joinWith(" ") ++ "}"
  | {data: MalQuote(x)} => "(quote " ++ printStr(x) ++ ")"
  | {data: MalQuasiQuote(x)} => "(quasiquote " ++ printStr(x) ++ ")"
  | {data: MalUnquote(x)} => "(unquote " ++ printStr(x) ++ ")"
  | {data: MalSpliceUnquote(x)} => "(splice-unquote " ++ printStr(x) ++ ")"
  | {data: MalDeref(x)} => "(deref " ++ printStr(x) ++ ")"
  | {data: MalWithMeta(x, y)} => "(with-meta " ++ printStr(y) ++ " " ++ printStr(x) ++ ")"
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
  | {data: MalKeyword(x)} => ":" ++ x
  | {data: MalAtom(x)} => x
  }
}
