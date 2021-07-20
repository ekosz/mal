module T = Types.Types
type replError = ReadError(Reader.readError) | EvalError(Eval.evalError)

module EnvMap = Belt.Map.String

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
    EnvMap.set(_, "+", numFun((a, b) => a + b)),
    EnvMap.set(_, "-", numFun((a, b) => a - b)),
    EnvMap.set(_, "*", numFun((a, b) => a * b)),
    EnvMap.set(_, "/", numFun((a, b) => a / b)),
  ]
  ->Js.Array2.reduce((acc, curried) => acc->curried, EnvMap.empty)
  ->Env.make(None)

let tryRunFunc = (f, args) => {
  try {
    Ok(f(args))
  } catch {
  | Js.Exn.Error(obj) =>
    switch obj->Js.Exn.message {
    | Some(message) => Error(Eval.RuntimeError(message))
    | None => Error(RuntimeError("JS error without message"))
    }
  | Invalid_argument(message) => Error(InvalidArgs(message))
  | _ => Error(RuntimeError("Unknown rescript error"))
  }
}

let rec doEval = (input, env) => {
  switch input {
  | T.List({value: []}) => input->evalAst(env)
  | T.List({value}) => value->evalList(input, env)
  | _ => input->evalAst(env)
  }
}
and evalAst = (ast, env) => {
  switch ast {
  | T.Symbol({value: x}) => env->Env.get(x)
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
and evalList = (nodes, input, env) => {
  switch nodes->Js.Array2.unsafe_get(0) {
  | T.Symbol({value: "def!"}) => nodes->evalDefFun(env)
  | T.Symbol({value: "let*"}) => nodes->evalLetFun(env)
  | _ =>
    input
    ->evalAst(env)
    ->Belt.Result.flatMap(nextAst => {
      switch nextAst {
      | T.List({value: []}) => Ok(nextAst)
      | T.List({value}) =>
        switch value->Js.Array2.unsafe_get(0) {
        | T.Fn({value: f}) => f->tryRunFunc(value->Js.Array2.sliceFrom(1))
        | _ => Ok(nextAst)
        }
      | _ => Ok(nextAst)
      }
    })
  }
}
and evalDefFun = (nodes, env) => {
  if nodes->Js.Array2.length !== 3 {
    Error(InvalidArgs("def! takes two arguments. A symbol and value"))
  } else {
    switch nodes->Js.Array2.unsafe_get(1) {
    | T.Symbol({value: sym}) =>
      nodes
      ->Js.Array2.unsafe_get(2)
      ->doEval(env)
      ->Belt.Result.map(val => {
        env->Env.set(sym, val)
        val
      })
    | _ => Error(InvalidArgs("First argument of def! must be a symbol"))
    }
  }
}
and evalLetFun = (nodes, env) => {
  if nodes->Js.Array2.length !== 3 {
    Error(InvalidArgs("let* requires 2 arguments"))
  } else {
    switch nodes->Js.Array2.unsafe_get(1) {
    | T.Vector({value: bindings})
    | T.List({value: bindings}) =>
      if bindings->Js.Array2.length->mod(2) !== 0 {
        Error(InvalidArgs("let* requires sets of key/value pairs"))
      } else {
        let newEnv = EnvMap.empty->Env.make(Some(env))
        let rec doBinding = bs => {
          if bs->Js.Array2.length === 0 {
            Ok()
          } else {
            switch bs->Js.Array2.unsafe_get(0) {
            | T.Symbol({value: key}) =>
              bs
              ->Js.Array2.unsafe_get(1)
              ->doEval(newEnv)
              ->Belt.Result.flatMap(val => {
                newEnv->Env.set(key, val)
                bs->Js.Array2.sliceFrom(2)->doBinding
              })
            | _ => Error(InvalidArgs("let* keys must be symbols"))
            }
          }
        }
        bindings
        ->doBinding
        ->Belt.Result.flatMap(() => nodes->Js.Array2.unsafe_get(2)->doEval(newEnv))
      }
    | _ => Error(InvalidArgs("First argument of let* must be a list or vector"))
    }
  }
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
