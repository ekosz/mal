let read = input => input->Reader.readStr
let eval = input => input
let print = output =>
  switch output {
  | Ok(ast) => ast->Printer.printStr
  | Error(Reader.UnmatchedString(pos)) =>
    "ERR: Found unbalanced string at pos " ++ pos->string_of_int
  | Error(BadInt(pos)) => "ERR: Cannot parse int at pos " ++ pos->string_of_int
  | Error(BadFloat(pos)) => "ERR: Cannot parse float at pos " ++ pos->string_of_int
  | Error(BadSymbol(pos)) => "ERR: Cannot parse symbol at pos " ++ pos->string_of_int
  | Error(SyntaxError(pos)) =>
    "ERR: Unknown syntax error somewhere around pos " ++ pos->string_of_int
  | Error(EOF) => "ERR: Unexpected EOF"
  }

let rep = (input: string) => input->read->eval->print

let rec main = () => {
  Readline.ask("user> ")
  ->Promise.thenResolve(x =>
    switch x {
    | EOF => ()
    | Response(res) => {
        let output = rep(res)
        Js.Console.log(output)
        main()->ignore
      }
    }
  )
  ->ignore
}

main()
