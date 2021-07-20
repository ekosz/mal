type t

let make: (Belt.Map.String.t<Types.mal_type>, option<t>) => t
let set: (t, string, Types.mal_type) => unit
let find: (t, string) => option<t>
let get: (t, string) => result<Types.mal_type, Eval.evalError>
