type replError = ReadError(Reader.readError)

let read = input =>
  switch input->Reader.readStr {
  | Ok(_) as x => x
  | Error(err) => Error(ReadError(err))
  }
let eval = input =>
  switch input {
  | Error(_) as x => x
  | Ok(ast) => Ok(ast)
  }
let print = output =>
  switch output {
  | Ok(ast) => ast->Printer.printAst
  | Error(ReadError(err)) => err->Printer.printReadError
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
