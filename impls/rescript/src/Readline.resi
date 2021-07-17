type rlResponse =
  | EOF
  | Response(string)

let ask: string => Promise.t<rlResponse>
