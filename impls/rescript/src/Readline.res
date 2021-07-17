type rli
type readline

@module("readline") external readline: rli = "default"
@send external createInterface: (rli, 'a, 'b) => readline = "createInterface"
@send
external on: (
  readline,
  @string
  [
    | #close(unit => unit)
    | #line(string => unit)
  ],
) => unit = "on"
@send external setPrompt: (readline, string) => unit = "setPrompt"
@send external prompt: readline => unit = "prompt"
@send external close: readline => unit = "close"

@val external process: 'a = "process"

type rlResponse =
  | EOF
  | Response(string)

let ask = (q: string): Promise.t<rlResponse> => {
  Promise.make((resolve, _reject) => {
    let rl = readline->createInterface(process["stdin"], process["stdout"])

    rl->setPrompt(q)
    rl->prompt

    rl->on(#close(_event => resolve(. EOF)))
    rl->on(
      #line(
        res => {
          resolve(. Response(res))
          rl->close
        },
      ),
    )
  })
}
