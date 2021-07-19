type rec ast = {loc: int, data: malType}
and hash = {
  key: ast,
  value: ast,
}
and malType =
  | MalList(array<ast>)
  | MalVector(array<ast>)
  | MalHashMap(array<hash>)
  | MalQuote(ast)
  | MalQuasiQuote(ast)
  | MalUnquote(ast)
  | MalSpliceUnquote(ast)
  | MalDeref(ast)
  | MalWithMeta(ast, ast)
  | MalString(string)
  | MalInt(int)
  | MalFloat(float)
  | MalTrue
  | MalFalse
  | MalNil
  | MalKeyword(string)
  | MalAtom(string)

type readError =
  | UnmatchedString(int)
  | BadInt(int)
  | BadFloat(int)
  | BadSymbol(int)
  | SyntaxError(int)
  | EOF

let readStr: string => result<ast, readError>
