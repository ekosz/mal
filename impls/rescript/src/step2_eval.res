module T = Types.Types
type replError = ReadError(Reader.readError) | EvalError(Eval.evalError)

module Env = Belt.Map.String

let push = (xs, x) => {
  let _ = xs->Js.Array2.push(x)
  xs
}

let numFun = f =>
  Types.fn(x =>
    switch x {
    | [T.Int(a), T.Int(b)] => T.Int(f(a, b))
    | _ => raise(Invalid_argument("Must be two int arguments"))
    }
  )

let replEnv =
  [
    Env.set(_, "+", numFun((a, b) => a + b)),
    Env.set(_, "-", numFun((a, b) => a - b)),
    Env.set(_, "*", numFun((a, b) => a * b)),
    Env.set(_, "/", numFun((a, b) => a / b)),
  ]->Js.Array2.reduce((acc, curried) => acc->curried, Env.empty)

let tryRunFunc = (f, args) => {
  try {
    Ok(f(args))
  } catch {
  | Js.Exn.Error(obj) =>
    switch obj->Js.Exn.message {
    | Some(message) => Error(Eval.RuntimeError(message))
    | None => Error(RuntimeError("JS error without message"))
    }
  | Invalid_argument(message) => Error(RuntimeError("Invalid Argument: " ++ message))
  | _ => Error(RuntimeError("Unknown rescript error"))
  }
}

let rec evalAst = (ast, env: Env.t<Types.mal_type>): result<Types.mal_type, Eval.evalError> => {
  switch ast {
  | T.Symbol({value: x}) =>
    switch env->Env.get(x) {
    | None => Error(Eval.SymbolNotFound(x))
    | Some(x) => Ok(x)
    }
  | T.List({value: xs}) => xs->Js.Array2.reduce((acc, x) => {
      switch acc {
      | Ok(T.List({value} as y)) =>
        x->doEval(env)->Belt.Result.map(nextNode => T.List({...y, value: value->push(nextNode)}))
      | x => x
      }
    }, Ok(Types.list([])))
  | T.Vector({value: xs}) => xs->Js.Array2.reduce((acc, x) => {
      switch acc {
      | Ok(T.Vector({value} as y)) =>
        x->doEval(env)->Belt.Result.map(nextNode => T.Vector({...y, value: value->push(nextNode)}))
      | x => x
      }
    }, Ok(Types.vector([])))
  | T.Map({value: x} as y) => {
      open Belt.Result

      x->Types.MalMap.fold((k, v, acc) => {
        acc->flatMap(m => {
          k
          ->doEval(env)
          ->flatMap(newKey => {
            v->doEval(env)->map(newVal => m->Types.MalMap.add(newKey, newVal, _))
          })
        })
      }, _, Ok(Types.MalMap.empty))->map(res => T.Map({...y, value: res}))
    }
  | _ => Ok(ast)
  }
}
and doEval = (input, env) => {
  input
  ->evalAst(env)
  ->Belt.Result.flatMap(result => {
    switch result {
    | T.List({value}) =>
      switch value->Belt.Array.get(0) {
      | Some(T.Fn({value: f})) => f->tryRunFunc(value->Js.Array2.sliceFrom(1))
      | _ => Ok(result)
      }
    | _ => Ok(result)
    }
  })
}

let read = input =>
  switch input->Reader.readStr {
  | Ok(_) as x => x
  | Error(err) => Error(ReadError(err))
  }

let eval = input =>
  input->Belt.Result.flatMap(ast =>
    switch ast->doEval(replEnv) {
    | Ok(_) as x => x
    | Error(x) => Error(EvalError(x))
    }
  )

let print = output =>
  switch output {
  | Ok(ast) => ast->Printer.printAst
  | Error(ReadError(err)) => err->Printer.printReadError
  | Error(EvalError(err)) => err->Printer.printEvalError
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
