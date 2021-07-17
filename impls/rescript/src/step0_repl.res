let read = (input: string) => input
let eval = (input: string) => input
let print = (input: string) => input

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
