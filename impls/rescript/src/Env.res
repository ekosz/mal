module Env = Belt.Map.String

type rec t = {
  outer: option<t>,
  env: ref<Env.t<Types.mal_type>>,
}

let make = (env, maybeOuter) => {outer: maybeOuter, env: ref(env)}
let set = (x, key, val) => {
  x.env := x.env.contents->Env.set(key, val)
  ()
}
let find = (originalEnv, key) => {
  let rec doFind = env => {
    env.env.contents->Env.has(key) ? Some(env) : env.outer->Belt.Option.flatMap(doFind)
  }

  doFind(originalEnv)
}
let get = (originalEnv, key) => {
  let rec doGet = env => {
    switch (env.env.contents->Env.get(key), env.outer) {
    | (Some(val), _) => Ok(val)
    | (None, Some(outer)) => doGet(outer)
    | (None, None) => Error(Eval.SymbolNotFound(key))
    }
  }

  doGet(originalEnv)
}
