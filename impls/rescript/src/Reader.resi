type rec ast = {loc: int, data: malType}
and malType =
  | MalList(array<ast>)
  | MalString(string)
  | MalInt(int)
  | MalFloat(float)
  | MalTrue
  | MalFalse
  | MalNil
  | MalSpliceUnquote
  | MalAtom(string)

type readError =
  | UnmatchedString(int)
  | BadInt(int)
  | BadFloat(int)
  | BadSymbol(int)
  | SyntaxError(int)
  | EOF

let readStr: string => result<ast, readError>
