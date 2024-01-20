let lama_to_json : L0.Program.t -> Yojson.Safe.t = 
  let rec helper = function 
  | L0.Program.Var s -> `Assoc [("kind", `String "Var"); "name", `String s]
  | Const n ->  `Assoc [("kind", `String "Const"); "value", `Int n]
  | Binop (op, l, r) -> 
      `Assoc  [ ("kind", `String "op"); ("name", `String op)
              ; ("left", helper l); ("right", helper r)
              ]
  in 

  helper

exception Bad_JSON_for_bytecode of string
let json_to_bytecode: Yojson.Safe.t -> L0.SM.t = 
  let rec helper = function 
  | `Int n
  | `Assoc [ ("kind", `String "Const"); ("value", `Int n)] -> L0.SM.CONST n
  | `String s
  | `Assoc [ ("kind", `String "Load"); ("value", `String s)] -> L0.SM.LD s
  | `Assoc [ ("kind", `String "Binop"); ("value", `String s)] -> L0.SM.BINOP s
  | _ -> raise (Bad_JSON_for_bytecode "неразобранный случай")
  in
  function 
  | `List xs -> List.map helper xs
  | _ -> raise (Bad_JSON_for_bytecode "ожидался список")