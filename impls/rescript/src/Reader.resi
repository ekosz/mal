type readError =
  | UnmatchedString(int)
  | BadInt(int)
  | BadFloat(int)
  | SyntaxError(int)
  | EOF

let readStr: string => result<Types.mal_type, readError>
