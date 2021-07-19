module rec Types: {
  type with_meta<'a> = {
    value: 'a,
    meta: option<Types.t>,
  }
  and t =
    | List(Types.with_meta<array<Types.t>>)
    | Vector(Types.with_meta<array<Types.t>>)
    | Map(Types.with_meta<MalMap.t<Types.t>>)
    | Symbol(Types.with_meta<string>)
    | Fn(Types.with_meta<array<Types.t> => Types.t>)
    | Keyword(string)
    | String(string)
    | Int(int)
    | Float(float)
    | Nil
    | True
    | False
} = Types
and MalValue: {
  type t = Types.t
  let compare: (t, t) => int
} = {
  type t = Types.t
  let compare = Pervasives.compare
}
and MalMap: Map.S with type key = MalValue.t = Map.Make(MalValue)

type mal_type = MalValue.t

let list = x => Types.List({value: x, meta: None})
let vector = x => Types.Vector({value: x, meta: None})
let map = x => Types.Map({value: x, meta: None})
let symbol = x => Types.Symbol({value: x, meta: None})
let fn = x => Types.Fn({value: x, meta: None})
