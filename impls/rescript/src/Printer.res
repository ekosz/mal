let rec printAst = (ast: Ast.t): string => {
  switch ast {
  | {data: MalList(x)} => "(" ++ x->Js.Array2.map(printAst)->Js.Array2.joinWith(" ") ++ ")"
  | {data: MalVector(x)} => "[" ++ x->Js.Array2.map(printAst)->Js.Array2.joinWith(" ") ++ "]"
  | {data: MalHashMap(x)} =>
    "{" ++
    x
    ->Js.Array2.map(({key, value}) => {
      printAst(key) ++ " " ++ printAst(value)
    })
    ->Js.Array2.joinWith(" ") ++ "}"
  | {data: MalQuote(x)} => "(quote " ++ printAst(x) ++ ")"
  | {data: MalQuasiQuote(x)} => "(quasiquote " ++ printAst(x) ++ ")"
  | {data: MalUnquote(x)} => "(unquote " ++ printAst(x) ++ ")"
  | {data: MalSpliceUnquote(x)} => "(splice-unquote " ++ printAst(x) ++ ")"
  | {data: MalDeref(x)} => "(deref " ++ printAst(x) ++ ")"
  | {data: MalWithMeta(x, y)} => "(with-meta " ++ printAst(y) ++ " " ++ printAst(x) ++ ")"
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
  | {data: MalSymbol(x)} => x
  }
}

let printReadError = (err: Reader.readError): string => {
  switch err {
  | Reader.UnmatchedString(pos) => "ERR: Found unbalanced string at pos " ++ pos->string_of_int
  | BadInt(pos) => "ERR: Cannot parse int at pos " ++ pos->string_of_int
  | BadFloat(pos) => "ERR: Cannot parse float at pos " ++ pos->string_of_int
  | SyntaxError(pos) => "ERR: Unknown syntax error somewhere around pos " ++ pos->string_of_int
  | EOF => "ERR: Unexpected EOF"
  }
}
