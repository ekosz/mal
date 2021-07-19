module T = Types.Types

type tokenType =
  | SpliceUnquote
  | OpenParen
  | CloseParen
  | OpenBrace
  | CloseBrace
  | OpenBracket
  | CloseBracket
  | Quote
  | BackQuote
  | Tilde
  | Caret
  | AtSign
  | String(string)
  | Int(string)
  | Float(string)
  | Comment(string)
  | Bare(string)

type token = {loc: int, data: tokenType}
type readError =
  | UnmatchedString(int)
  | BadInt(int)
  | BadFloat(int)
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
  | '0' .. '9' => true
  | _ => false
  }

let tokenize = (str: string): result<array<token>, readError> => {
  let rec doTokenize = (idx: int, currentToken: option<token>, tokens: array<token>) => {
    if idx >= str->String.length {
      switch currentToken {
      | None => Ok(tokens)
      | Some({loc, data: String(_)}) => Error(UnmatchedString(loc))
      | Some({data: Float(_)} as x)
      | Some({data: Bare(_)} as x)
      | Some({data: Int(_)} as x) =>
        Ok(tokens->push(x))
      | Some({data: Comment(_)}) => Ok(tokens)
      | Some(_) => Js.Exn.raiseError("Ended tokenize with bad token")
      }
    } else {
      switch (currentToken, str->String.get(idx)) {
      // Whitespace
      | (None, _ as x) if x->isWhiteSpace => doTokenize(idx + 1, None, tokens)
      // SpliceUnquote "~@"
      | (None, '~') if str->String.get(idx + 1) === '@' =>
        doTokenize(idx + 2, None, tokens->push({loc: idx, data: SpliceUnquote}))
      // Special Chars
      | (None, '(') => doTokenize(idx + 1, None, tokens->push({loc: idx, data: OpenParen}))
      | (None, ')') => doTokenize(idx + 1, None, tokens->push({loc: idx, data: CloseParen}))
      | (None, '[') => doTokenize(idx + 1, None, tokens->push({loc: idx, data: OpenBracket}))
      | (None, ']') => doTokenize(idx + 1, None, tokens->push({loc: idx, data: CloseBracket}))
      | (None, '{') => doTokenize(idx + 1, None, tokens->push({loc: idx, data: OpenBrace}))
      | (None, '}') => doTokenize(idx + 1, None, tokens->push({loc: idx, data: CloseBrace}))
      | (None, '\'') => doTokenize(idx + 1, None, tokens->push({loc: idx, data: Quote}))
      | (None, '`') => doTokenize(idx + 1, None, tokens->push({loc: idx, data: BackQuote}))
      | (None, '~') => doTokenize(idx + 1, None, tokens->push({loc: idx, data: Tilde}))
      | (None, '^') => doTokenize(idx + 1, None, tokens->push({loc: idx, data: Caret}))
      | (None, '@') => doTokenize(idx + 1, None, tokens->push({loc: idx, data: AtSign}))
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
      // Enter Bare
      | (None, _ as x) =>
        doTokenize(idx + 1, Some({loc: idx, data: Bare(String.make(1, x))}), tokens)
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
      | (Some({data: Int(_)} as y), z) if z->isSpecialChar => doTokenize(idx, None, tokens->push(y))
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
        doTokenize(idx, None, tokens->push(y))
      | (Some({loc, data: Float(_)}), _) => Error(BadFloat(loc))
      // Bare
      | (Some({data: Bare(_)} as y), z) if z->isSpecialChar =>
        doTokenize(idx, None, tokens->push(y))
      | (Some({data: Bare(_)} as y), z) if z->isWhiteSpace =>
        doTokenize(idx + 1, None, tokens->push(y))
      | (Some({data: Bare(x)} as y), z) =>
        doTokenize(idx + 1, Some({...y, data: Bare(x->concat(z))}), tokens)
      | (Some(x), _) => {
          Logger.debug2("Bad currentToken at " ++ idx->string_of_int, x)
          Js.Exn.raiseError("Got bad currentToken while tokenizing")
        }
      }
    }
  }

  doTokenize(0, None, [])
}

type reader = {pos: int, tokens: array<token>}
type readRes = result<(Types.Types.t, reader), readError>

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
  let read = (token: token): Types.mal_type =>
    switch token {
    | {data: String(x)} => {
        Logger.debug2("Cleaning string", x)
        T.String(
          // \" -> "
          // \\ -> \
          // \n -> <new line>
          x->Js.String2.unsafeReplaceBy1(%re("/\\\\(.)/g"), (_, c, _, _) => c === "n" ? "\n" : c),
        )
      }
    | {data: Int(x)} => T.Int(x->int_of_string)
    | {data: Float(x)} => T.Float(x->float_of_string)
    | {data: Bare("true")} => T.True
    | {data: Bare("false")} => T.False
    | {data: Bare("nil")} => T.Nil
    | {data: Bare(x)} if x->String.get(0) === ':' => T.Keyword(x->Js.String2.substr(~from=1))
    | {data: Bare(x)} => Types.symbol(x)
    | _ => Js.Exn.raiseError("Called readAtom with non-atom token")
    }
  switch reader->next {
  | (None, _) => Error(EOF)
  | (Some(token), nextReader) => Ok((token->read, nextReader))
  }
}

type hash = {key: Types.mal_type, value: Types.mal_type}
let rec hahesToMap = (target, source, loc) =>
  switch source {
  | list{} => Types.map(target)
  | list{{key, value}, ...rest} => hahesToMap(Types.MalMap.add(key, value, target), rest, loc)
  }

let rec readForm = (reader: reader): readRes => {
  switch reader->peek {
  | None => Error(EOF)
  | Some({data: OpenParen}) => reader->readList
  | Some({data: OpenBracket}) => reader->readVector
  | Some({data: OpenBrace} as x) => reader->readHashMap(x)
  | Some({data: Quote}) => reader->readQuote("quote")
  | Some({data: BackQuote}) => reader->readQuote("quasiquote")
  | Some({data: Tilde}) => reader->readQuote("unquote")
  | Some({data: AtSign}) => reader->readQuote("deref")
  | Some({data: SpliceUnquote}) => reader->readQuote("splice-unquote")
  | Some({data: Caret}) => reader->readWithMeta
  | Some(_) => reader->readAtom
  }
}
and readList = (reader): readRes => {
  let rec doReadList = (reader, nodes: array<Types.mal_type>): readRes => {
    switch reader->peek {
    | None => Error(EOF)
    | Some({data: CloseParen}) => Ok(Types.list(nodes), reader->inc)
    | Some(_) =>
      switch reader->readForm {
      | Error(_) as x => x
      | Ok(node, nextReader) => nextReader->doReadList(nodes->push(node))
      }
    }
  }
  doReadList(reader->inc, [])
}
and readVector = (reader): readRes => {
  let rec doReadVector = (reader, nodes: array<Types.mal_type>): readRes => {
    switch reader->peek {
    | None => Error(EOF)
    | Some({data: CloseBracket}) => Ok(Types.vector(nodes), reader->inc)
    | Some(_) =>
      switch reader->readForm {
      | Error(_) as x => x
      | Ok(node, nextReader) => nextReader->doReadVector(nodes->push(node))
      }
    }
  }
  doReadVector(reader->inc, [])
}
and readHashMap = (reader, node): readRes => {
  let rec doReadHashMap = (reader, nodes: list<hash>): readRes => {
    switch reader->peek {
    | None => Error(EOF)
    | Some({data: CloseBrace}) => Ok(Types.MalMap.empty->hahesToMap(nodes, node.loc), reader->inc)
    | Some(_) =>
      switch reader->readForm {
      | Error(_) as x => x
      | Ok(key, nextReader) =>
        switch nextReader->readForm {
        | Error(_) as x => x
        | Ok(value, finalReader) =>
          finalReader->doReadHashMap(list{{key: key, value: value}, ...nodes})
        }
      }
    }
  }
  doReadHashMap(reader->inc, list{})
}
and readQuote = (reader, name): readRes => {
  switch reader->inc->readForm {
  | Ok(nextNode, nextReader) => Ok(Types.list([Types.symbol(name), nextNode]), nextReader)
  | Error(_) as x => x
  }
}
and readWithMeta = (reader): readRes => {
  switch reader->inc->readForm {
  | Ok(meta, nextReader) =>
    switch nextReader->readForm {
    | Ok(value, finalReader) =>
      Ok(Types.list([Types.symbol("with-meta"), value, meta]), finalReader)
    | Error(_) as x => x
    }
  | Error(_) as x => x
  }
}

let readStr = (input: string): result<Types.mal_type, readError> => {
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
