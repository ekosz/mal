module T = Types.Types

let rec printAst = (ast: Types.mal_type): string => {
  switch ast {
  | T.List({value: x}) => "(" ++ x->Js.Array2.map(printAst)->Js.Array2.joinWith(" ") ++ ")"
  | T.Vector({value: x}) => "[" ++ x->Js.Array2.map(printAst)->Js.Array2.joinWith(" ") ++ "]"
  | T.Map({value: x}) => "{" ++ Types.MalMap.fold((key, val, acc) => {
      acc ++ (acc === "" ? "" : " ") ++ printAst(key) ++ " " ++ printAst(val)
    }, x, "") ++ "}"
  | T.String(x) =>
    "\"" ++
    {
      open Js.String2
      x
      ->replaceByRe(%re("/\\\\/g"), "\\\\")
      ->replaceByRe(%re("/\"/g"), "\\\"")
      ->replaceByRe(%re("/\\n/g"), "\\n")
    } ++ "\""
  | T.Symbol({value: x}) => x
  | T.Int(x) => x->string_of_int
  | T.Float(x) => x->Js.Float.toString
  | T.True => "true"
  | T.False => "false"
  | T.Nil => "nil"
  | T.Keyword(x) => ":" ++ x
  | T.Fn(_) => ""
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

let printEvalError = (err: Eval.evalError): string => {
  switch err {
  | Eval.SymbolNotFound(name) => `ERR: Symbol '${name}' not found`
  | RuntimeError(message) => `Runtime ERR: ${message}`
  | InvalidArgs(message) => `Invalid Agument ERR: ${message}`
  }
}
