@val external process: 'a = "process"

let logLevel: string = process["env"]["LOG_LEVEL"]
let logLevelInt = switch logLevel {
| "debug" => 0
| "info" => 1
| "warn" => 2
| "error" => 3
| _ => 1
}

let debug = str => {
  if logLevelInt <= 0 {
    Js.Console.log(str)
  }
}
let debug2 = (str, obj) => {
  if logLevelInt <= 0 {
    Js.Console.log2(str, obj)
  }
}

let info = str => {
  if logLevelInt <= 1 {
    Js.Console.log(str)
  }
}

let warn = str => {
  if logLevelInt <= 2 {
    Js.Console.log(str)
  }
}

let error = str => {
  if logLevelInt <= 2 {
    Js.Console.error(str)
  }
}
