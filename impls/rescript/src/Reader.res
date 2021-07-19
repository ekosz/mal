type tokenType =
  | SpliceUnquote
  | SpecialChar(char)
  | String(string)
  | Int(string)
  | Float(string)
  | Comment(string)
  | Symbol(string)

type token = {loc: int, data: tokenType}
type readError =
  | UnmatchedString(int)
  | BadInt(int)
  | BadFloat(int)
  | BadSymbol(int)
  | SyntaxError(int)
  | EOF

let push = (xs, x) => {
  let _ = xs->Js.Array2.push(x)
  xs
}
let concat = (xs, x) => {
  xs->Js.String2.concat(String.make(1, x))
}

let isWhiteSpace = x =>
  switch x {
  | ' ' => true
  | '\n' => true
  | '\t' => true
  | ',' => true
  | _ => false
  }

let isSpecialChar = x =>
  switch x {
  | '[' => true
  | ']' => true
  | '{' => true
  | '}' => true
  | '(' => true
  | ')' => true
  | '\'' => true
  | '`' => true
  | '~' => true
  | '^' => true
  | '@' => true
  | _ => false
  }

let isNumericChar = x =>
  switch x {
  | '0' => true
  | '1' => true
  | '2' => true
  | '3' => true
  | '4' => true
  | '5' => true
  | '6' => true
  | '7' => true
  | '8' => true
  | '9' => true
  | _ => false
  }

let tokenize = (str: string): result<array<token>, readError> => {
  let rec doTokenize = (idx: int, currentToken: option<token>, tokens: array<token>) => {
    if idx >= str->String.length {
      switch currentToken {
      | None => Ok(tokens)
      | Some({loc, data: String(_)}) => Error(UnmatchedString(loc))
      | Some({data: SpliceUnquote}) => Js.Exn.raiseError("Ended tokenize with SpliceUnquote token")
      | Some({data: SpecialChar(_)}) => Js.Exn.raiseError("Ended tokenize with SpecialChar token")
      | Some({data: Float(_)} as x)
      | Some({data: Symbol(_)} as x)
      | Some({data: Int(_)} as x) =>
        Ok(tokens->push(x))
      | Some({data: Comment(_)}) => Ok(tokens)
      }
    } else {
      switch (currentToken, str->String.get(idx)) {
      // Whitespace
      | (None, _ as x) if x->isWhiteSpace => doTokenize(idx + 1, None, tokens)
      // SpliceUnquote "~@"
      | (None, '~') if str->String.get(idx + 1) === '@' =>
        doTokenize(idx + 2, None, tokens->push({loc: idx, data: SpliceUnquote}))
      // SpecialChar
      | (None, _ as x) if x->isSpecialChar =>
        doTokenize(idx + 1, None, tokens->push({loc: idx, data: SpecialChar(x)}))
      // Enter String
      | (None, '"') => doTokenize(idx + 1, Some({loc: idx, data: String("")}), tokens)
      // Enter Comment
      | (None, ';') => doTokenize(idx + 1, Some({loc: idx, data: Comment("")}), tokens)
      // Enter Int
      | (None, _ as x) if x->isNumericChar =>
        doTokenize(idx + 1, Some({loc: idx, data: Int(String.make(1, x))}), tokens)
      | (None, '-' as x)
        if idx !== str->Js.String2.length - 1 && str->String.get(idx + 1)->isNumericChar =>
        doTokenize(idx + 1, Some({loc: idx, data: Int(String.make(1, x))}), tokens)
      // Enter Symbol
      | (None, _ as x) =>
        doTokenize(idx + 1, Some({loc: idx, data: Symbol(String.make(1, x))}), tokens)
      // Enter Float
      | (Some({data: Int(x)} as y), '.' as z) =>
        doTokenize(idx + 1, Some({...y, data: Float(x->concat(z))}), tokens)

      | (Some({data: String(x)} as y), '\\' as z) if idx !== str->Js.String2.length - 1 => {
          Logger.debug2("Detecting escape", idx)
          let nextChar = str->String.get(idx + 1)
          doTokenize(idx + 2, Some({...y, data: String(x->concat(z)->concat(nextChar))}), tokens)
        }
      | (Some({data: String(_)} as x), '"') => doTokenize(idx + 1, None, tokens->push(x))
      | (Some({data: String(x)} as y), z) =>
        doTokenize(idx + 1, Some({...y, data: String(x->concat(z))}), tokens)
      // Comment
      | (Some({data: Comment(_)}), '\n') => doTokenize(idx + 1, None, tokens)
      | (Some({data: Comment(x)} as y), z) =>
        doTokenize(idx + 1, Some({...y, data: Comment(x->concat(z))}), tokens)
      // Int
      | (Some({data: Int(x)} as y), z) if z->isNumericChar =>
        doTokenize(idx + 1, Some({...y, data: Int(x->concat(z))}), tokens)
      // Allow '_' in integers. Just skips
      | (Some({data: Int(_)}), '_') => doTokenize(idx + 1, currentToken, tokens)
      | (Some({data: Int(_)} as y), ';') =>
        doTokenize(idx + 1, Some({loc: idx, data: Comment("")}), tokens->push(y))
      | (Some({data: Int(_)} as y), z) if z->isWhiteSpace =>
        doTokenize(idx + 1, None, tokens->push(y))
      | (Some({data: Int(_)} as y), z) if z->isSpecialChar =>
        doTokenize(idx + 1, None, tokens->push(y)->push({loc: idx, data: SpecialChar(z)}))
      | (Some({loc, data: Int(_)}), _) => Error(BadInt(loc))
      // Float
      | (Some({data: Float(x)} as y), z) if z->isNumericChar =>
        doTokenize(idx + 1, Some({...y, data: Float(x->concat(z))}), tokens)
      | (Some({data: Float(_)} as y), ';') =>
        doTokenize(idx + 1, Some({loc: idx, data: Comment("")}), tokens->push(y))
      | (Some({data: Float(_)} as y), z) if z->isWhiteSpace =>
        doTokenize(idx + 1, None, tokens->push(y))
      | (Some({data: Float(_)} as y), z) if z->isWhiteSpace =>
        doTokenize(idx + 1, None, tokens->push(y))
      | (Some({data: Float(_)} as y), z) if z->isSpecialChar =>
        doTokenize(idx + 1, None, tokens->push(y)->push({loc: idx, data: SpecialChar(z)}))
      | (Some({loc, data: Float(_)}), _) => Error(BadFloat(loc))
      // Symbol
      | (Some({data: Symbol(_)} as y), z) if z->isSpecialChar =>
        doTokenize(idx + 1, None, tokens->push(y)->push({loc: idx, data: SpecialChar(z)}))
      | (Some({data: Symbol(_)} as y), z) if z->isWhiteSpace =>
        doTokenize(idx + 1, None, tokens->push(y))
      | (Some({data: Symbol(x)} as y), z) =>
        doTokenize(idx + 1, Some({...y, data: Symbol(x->concat(z))}), tokens)
      // Shouldn't get to these
      | (Some({data: SpecialChar(_)}), _) => Js.Exn.raiseError("SpecialChar durring tokenize")
      | (Some({data: SpliceUnquote}), _) => Js.Exn.raiseError("SpliceUnquote durring tokenize")
      }
    }
  }

  doTokenize(0, None, [])
}

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

type reader = {pos: int, tokens: array<token>}
type readRes = result<(ast, reader), readError>

let make = input =>
  switch tokenize(input) {
  | Ok(tokens) => {
      Logger.debug2("tokens", tokens)
      Ok({pos: 0, tokens: tokens})
    }
  | Error(error) => Error(error)
  }
let peek = reader => reader.tokens->Belt.Array.get(reader.pos)
let next = reader => (reader->peek, {...reader, pos: reader.pos + 1})
let inc = reader => {...reader, pos: reader.pos + 1}
let readAtom = (reader: reader): readRes => {
  let read = (token: token) =>
    switch token {
    | {loc, data: String(x)} => {
        Logger.debug2("Cleaning string", x)
        {
          loc: loc,
          data: MalString(
            // \" -> "
            // \\ -> \
            // \n -> <new line>
            x->Js.String2.unsafeReplaceBy1(%re("/\\\\(.)/g"), (_, c, _, _) => c === "n" ? "\n" : c),
          ),
        }
      }
    | {loc, data: Int(x)} => {loc: loc, data: MalInt(x->int_of_string)}
    | {loc, data: Float(x)} => {loc: loc, data: MalFloat(x->float_of_string)}
    | {loc, data: SpecialChar(x)} => {loc: loc, data: MalAtom(String.make(1, x))}
    | {loc, data: SpliceUnquote} => {loc: loc, data: MalAtom("~@")}
    | {data: Comment(_)} => Js.Exn.raiseError("Comment found in tokens")
    | {loc, data: Symbol("true")} => {loc: loc, data: MalTrue}
    | {loc, data: Symbol("false")} => {loc: loc, data: MalFalse}
    | {loc, data: Symbol("nil")} => {loc: loc, data: MalNil}
    | {loc, data: Symbol(x)} if x->String.get(0) === ':' => {
        loc: loc,
        data: MalKeyword(x->Js.String2.substr(~from=1)),
      }
    | {loc, data: Symbol(x)} => {loc: loc, data: MalAtom(x)}
    }
  switch reader->next {
  | (None, _) => Error(EOF)
  | (Some(token), nextReader) => Ok((token->read, nextReader))
  }
}

let rec readForm = (reader: reader): readRes => {
  switch reader->peek {
  | None => Error(EOF)
  | Some({data: SpecialChar('(')} as x) => reader->readList(x)
  | Some({data: SpecialChar('[')} as x) => reader->readVector(x)
  | Some({data: SpecialChar('{')} as x) => reader->readHashMap(x)
  | Some({data: SpecialChar('\'')} as x) => reader->readQuote(x)
  | Some({data: SpecialChar('`')} as x) => reader->readQuasiQuote(x)
  | Some({data: SpecialChar('~')} as x) => reader->readUnquote(x)
  | Some({data: SpecialChar('@')} as x) => reader->readDeref(x)
  | Some({data: SpecialChar('^')} as x) => reader->readWithMeta(x)
  | Some({data: SpliceUnquote} as x) => reader->readSpliceUnquote(x)
  | Some(_) => reader->readAtom
  }
}
and readList = (reader, node): readRes => {
  let rec doReadList = (reader, nodes: array<ast>) => {
    switch reader->readForm {
    | Error(_) as x => x
    | Ok({data: MalAtom(")")}, nextReader) => Ok({loc: node.loc, data: MalList(nodes)}, nextReader)
    | Ok(node, nextReader) => nextReader->doReadList(nodes->push(node))
    }
  }
  doReadList(reader->inc, [])
}
and readVector = (reader, node): readRes => {
  let rec doReadVector = (reader, nodes: array<ast>) => {
    switch reader->readForm {
    | Error(_) as x => x
    | Ok({data: MalAtom("]")}, nextReader) =>
      Ok({loc: node.loc, data: MalVector(nodes)}, nextReader)
    | Ok(node, nextReader) => nextReader->doReadVector(nodes->push(node))
    }
  }
  doReadVector(reader->inc, [])
}
and readHashMap = (reader, node): readRes => {
  let rec doReadHashMap = (reader, nodes: array<hash>) => {
    switch reader->readForm {
    | Error(_) as x => x
    | Ok({data: MalAtom("}")}, nextReader) =>
      Ok({loc: node.loc, data: MalHashMap(nodes)}, nextReader)
    | Ok(key, nextReader) =>
      switch nextReader->readForm {
      | Error(_) as x => x
      | Ok(value, finalReader) => finalReader->doReadHashMap(nodes->push({key: key, value: value}))
      }
    }
  }
  doReadHashMap(reader->inc, [])
}
and readQuote = (reader, node): readRes => {
  switch reader->inc->readForm {
  | Ok(nextNode, nextReader) => Ok({loc: node.loc, data: MalQuote(nextNode)}, nextReader)
  | Error(_) as x => x
  }
}
and readQuasiQuote = (reader, node): readRes => {
  switch reader->inc->readForm {
  | Ok(nextNode, nextReader) => Ok({loc: node.loc, data: MalQuasiQuote(nextNode)}, nextReader)
  | Error(_) as x => x
  }
}
and readUnquote = (reader, node): readRes => {
  switch reader->inc->readForm {
  | Ok(nextNode, nextReader) => Ok({loc: node.loc, data: MalUnquote(nextNode)}, nextReader)
  | Error(_) as x => x
  }
}
and readSpliceUnquote = (reader, node): readRes => {
  switch reader->inc->readForm {
  | Ok(nextNode, nextReader) => Ok({loc: node.loc, data: MalSpliceUnquote(nextNode)}, nextReader)
  | Error(_) as x => x
  }
}
and readDeref = (reader, node): readRes => {
  switch reader->inc->readForm {
  | Ok(nextNode, nextReader) => Ok({loc: node.loc, data: MalDeref(nextNode)}, nextReader)
  | Error(_) as x => x
  }
}
and readWithMeta = (reader, node): readRes => {
  switch reader->inc->readForm {
  | Ok(meta, nextReader) =>
    switch nextReader->readForm {
    | Ok(value, finalReader) => Ok({loc: node.loc, data: MalWithMeta(meta, value)}, finalReader)
    | Error(_) as x => x
    }
  | Error(_) as x => x
  }
}

let readStr = (input: string): result<ast, readError> => {
  Logger.debug2("readStr", input)
  switch input->make {
  | Ok(reader) =>
    switch reader->readForm {
    | Ok((x, finalReader)) =>
      if (
        finalReader.pos === finalReader.tokens->Js.Array2.length ||
          finalReader.pos === finalReader.tokens->Js.Array2.length - 1
      ) {
        Ok(x)
      } else {
        Error(SyntaxError(finalReader.tokens[finalReader.pos].loc))
      }
    | Error(_) as x => x
    }
  | Error(_) as x => x
  }
}
