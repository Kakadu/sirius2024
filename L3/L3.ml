open GT

let failwiths fmt = Printf.kprintf failwith fmt

module Algebra =
  struct

    let if_bool = function
    | 0 -> false
    | 1 -> true
    | x -> failwith "non-boolean value"

    let evalOp =
      let of_bool f x y = if f x y then 1 else 0 in
      function
      | "|"  -> of_bool (fun x y -> if_bool x || if_bool y)
      | "&"  -> of_bool (fun x y -> if_bool x && if_bool y)
      | "<"  -> of_bool ( <  )
      | "<=" -> of_bool ( <= )
      | ">"  -> of_bool ( >  )
      | ">=" -> of_bool ( >= )
      | "==" -> of_bool ( =  )
      | "<>" -> of_bool ( <> )
      | "+"  -> ( +  )
      | "-"  -> ( -  ) 
      | "*"  -> ( *  )
      | "/"  -> ( /  )
      | "%"  -> ( mod )
      | op   -> failwith (Printf.sprintf "unrecognized operator \"%s\"" op)
  
  end

module Program =
  struct

    @type t =
    | Var    of string
    | Const  of int           
    | Binop  of string * t * t                    
    | Skip
    | Read  
    | Write  of t
    | If     of t * t * t
    | Seq    of t * t
    | Call   of t * t list
    | Fun    of string list * t
    | Let    of string * t * t with show
      
    exception Undefined of string
          
    let empty  x        = raise (Undefined x)
    let update st x n y = if y = x then n else st y
    let undefine st x y = if y = x then raise (Undefined x) else st y
    let defined state x =
      try ignore (state x); true with Undefined _ -> false
        
    let rec alloc st = function
    | [], []       -> st
    | x::xs, n::ns -> alloc (update st x n) (xs, ns)
    | _            -> failwith "wrong number of actual parameters in supplied in a function call"
                      
    let restore st fargs st' =
      List.fold_left
        (fun st' name ->
           if defined st name
           then update st' name (st name)
           else undefine st' name
        )
        st'
        fargs
     
    let eval i p =
      let rec evals c es =
        let c, vs = 
          List.fold_left
            (fun (c, acc) e ->
               let c', n = eval c e in
               (c', n :: acc)
            )
            (c, [])
            es
        in
        (c, List.rev vs)
      and eval ((st, i, o) as c) = function
      | Var x -> (c, st x)
      | Const n -> (c, `I n)
      | Binop (op, l, r) ->
        let c', vs = evals c [l; r] in
        (match vs with
         | [`I l; `I r] -> c', `I (Algebra.evalOp op l r)
         | _            -> failwith (Printf.sprintf "non-primitive values in binary op %S" op)          
        )
      | Skip -> (c, `U) 
      | Read ->
        (match i with
         | n :: i' -> ((st, i', o), `I n)
         | _       -> failwith "input stream is exhausted"
        )      
      | Write e ->
        let ((st', i', o'), n) = eval c e in
        (match n with
         | `I n -> ((st', i', n :: o), `U)
         | _    -> failwith "non-integer value in \"write\""
        )                   
      | If (f, t, e) ->
        let c', fv = eval c f in
        (match fv with
         | `I fv -> if Algebra.if_bool fv
                    then eval c' t
                    else eval c' e
         | _ -> failwith "non-primitive value in \"if\""
        )
      | Seq (s1, s2)  ->
        let c', [v1; v2] = evals c [s1; s2] in
        (match v1 with
         | `U -> c', v2
         | _  -> failwith "non-unit value in sequential expression"
        )
      | Call (f, es) ->
        let (st', i', o'), fv :: args = evals c (f :: es) in
        (match fv with
         | `C (fargs, st, s) ->
           let st'' = alloc st (fargs, args) in
           let (st''', i'', o''), v = eval (st'', i', o') s in
           (restore st fargs st''', i'', o''), v
           
         | _ -> failwith "a callee did not evaluate to a closure"
        )
        
      | Fun (args, s) -> (c, `C (args, st, s))
                         
      | Let (x, e, f) ->
        let (st', i', o'), v = eval c e in
        eval (update st' x v, i', o') f
      
      in
      let ((_, _, o), _) = eval (empty, i, []) p in
      List.rev o
                           
  end
(*
module SM =
  struct

    @type insn =
    | CONST of int
    | LD    of string
    | BINOP of string
    | ST    of string
    | READ
    | WRITE
    | JMP   of string
    | JZ    of string
    | JNZ   of string
    | LABEL of string
    | BEGIN of string list
    | END
    | CALL  of string
    with show

    @type t = insn list with show

    let compile_expr p =
      let rec compile acc = function
      | Program.Expr.Var   x          -> LD x :: acc
      | Program.Expr.Const n          -> CONST n :: acc                       
      | Program.Expr.Binop (op, l, r) -> compile (compile (BINOP op :: acc) r) l
      in
      List.rev @@ compile [] p

    let eval i p =
      let lookup =
        let module M = Map.Make (String) in
        let rec fill m = function
        | [] -> m
        | LABEL l :: tl ->
          (match M.find_opt l m with
           | Some _ -> failwith (Printf.sprintf "duplicate label %s" l)
           | _      -> fill (M.add l tl m) tl
          )
        | _ :: tl -> fill m tl
        in
        let m = fill M.empty p in
        fun l ->
          match M.find_opt l m with
          | None   -> failwith (Printf.sprintf "undefined label %s" l)
          | Some p -> p
      in
      let rec eval ((st, s, cs, i, o) as c) = function
        | [] -> c
        | LD    x  :: tl -> eval (st, st x :: s, cs, i, o) tl
        | CONST n  :: tl -> eval (st, n    :: s, cs, i, o) tl
        | BINOP op :: tl ->
          (match s with
           | x :: y :: s' -> eval (st, Algebra.evalOp op y x :: s', cs, i, o) tl
           | _            ->
             failwiths "exhausted stack at BINOP %s: %S" op ((show(list) (show(int))) s)
          )
        | ST x :: tl ->
          (match s with
           | n :: s' -> eval (Program.update st x n, s', cs, i, o) tl
           | _       -> failwith (Printf.sprintf "exhausted stack at ST %s" x)
          )
        | READ :: tl ->
          (match i with
           | n :: i' -> eval (st, n :: s, cs, i', o) tl
           | _       -> failwith "exhausted input stream"
          )
        | WRITE :: tl ->
          (match s with
           | n :: s' -> eval (st, s', cs, i, n :: o) tl
           | _       -> failwith "exhausted stack at WRITE"
          )
        | JMP l :: _ ->
          eval c (lookup l)            
        | JZ l :: tl ->
          (match s with
           | n :: s' ->
             eval (st, s', cs, i, o) @@
             if Algebra.if_bool n
             then tl
             else lookup l
           | _ -> failwith (Printf.sprintf "exhausted stack at JZ %s" l)
          )
        | JNZ l :: tl ->
          (match s with
           | n :: s' ->
             eval (st, s', cs, i, o) @@
             if Algebra.if_bool n
             then lookup l
             else tl
           | _ -> failwith (Printf.sprintf "exhausted stack at JZ %s" l)
          )          
        | LABEL _ :: tl -> eval c tl
        | CALL f :: tl ->
          eval (st, s, ((fun _ -> st), tl) :: cs, i, o) (lookup f)
        | BEGIN fargs :: tl ->
          let (_, ret) :: cs = cs in
          let st', s' =
            List.fold_left
              (fun (st', s') arg ->
                 match s' with
                 | a :: s'' -> Program.update st' arg a, s''
                 | _        -> failwiths "exhausted stack at \"BEGIN %s\"" @@ (show(list) (show(string))) fargs                
              )
              (st, s)
              fargs
          in
          eval (st', s', (Program.restore st fargs, ret) :: cs, i, o) tl
        | END :: _ ->
          (match cs with
           | [] -> c
           | (restore, p) :: cs' ->
             eval (restore st, s, cs', i, o) p
          )
      in
      let _, _, _, _, o = eval (Program.empty, [], [], i, []) p in
      List.rev o
      
  end
  
type module_ = Program.t list * Program.t
*)
module Parser =
  struct

    open Ostap
    open Ostap.Util

    let expression primary =
      let binop op x y = Program.Binop (op, x, y) in
      expr
        (fun x -> x)
        [|
          (* Disjunction *)
          `Lefta, [ostap ("|"), binop "|"];

          (* Conjunction *)
          `Lefta, [ostap ("&"), binop "&"];

          (* Relational operators *)
          `Nona, [ostap ("<="), binop "<=";
                  ostap ("<" ), binop "<";
                  ostap (">="), binop ">=";
                  ostap (">" ), binop ">";
                  ostap ("=="), binop "==";
                  ostap ("<>"), binop "<>";
                 ];

          (* Additive operators *)
          `Lefta, [ostap ("+"), binop "+";
                   ostap ("-"), binop "-";
                  ];

          (* Multiplicative operators *)
          `Lefta, [ostap ("*"), binop "*";
                   ostap ("/"), binop "/";
                   ostap ("%"), binop "%";
                  ];
        |]
        primary
        
    ostap (
      primary: let_expr;

      let_expr:
       "let" x:LIDENT "=" e:expr "in" f:let_expr {Program.Let (x, e, f)}
      | seq_expr;

      seq_expr: s:!(Util.listBy)[ostap (";")][call_expr] {
          let h::t = List.rev s in          
          List.fold_right (fun s acc-> Program.Seq (s, acc)) (List.rev t) h
      };
        
      call_expr:
        f:simple_expr args:(-"(" !(Util.list0 expr) -")")* {
        List.fold_left
          (fun e args ->
             Program.Call (e, args)
          )
          f
          args
      };
          
      simple_expr:
        x:DECIMAL                             {Program.Const x}
      | x:LIDENT                              {Program.Var x}
      | "skip"                                {Program.Skip}
      | "if" c:expr "then" t:expr
                    "else" e:expr
        "fi"                                  {Program.If (c, t, e)}
      | "read"                                {Program.Read}
      | "write" "(" e:expr ")"                {Program.Write (e)}
      | "{" args:LIDENT* "->" body:expr "}"   {Program.Fun (args, body)}
      | -"(" expr -")";
      
      expr: expression[primary];
      
      input: DECIMAL*
    )

    let parse_input =
      let kws = [] in
      fun s ->
        parse
          (object
            inherit Matcher.t s
            inherit Util.Lexers.decimal s
            inherit! Util.Lexers.skip [
                Matcher.Skip.whitespaces " \t\n\r";
                Matcher.Skip.lineComment "--";
                Matcher.Skip.nestedComment "(*" "*)"
              ] s
            inherit Util.Lexers.lident kws s
          end
          )
          (ostap (input -EOF))

    let parse =
      let kws = ["skip"; "if"; "fi"; "then"; "else"; "read"; "write"; "let"; "in"] in
      fun s ->
        parse
          (object
            inherit Matcher.t s
            inherit Util.Lexers.decimal s
            inherit! Util.Lexers.skip [
                Matcher.Skip.whitespaces " \t\n\r";
                Matcher.Skip.lineComment "--";
                Matcher.Skip.nestedComment "(*" "*)"
              ] s
            inherit Util.Lexers.lident kws s
          end
          )
          (ostap (expr -EOF))
 
  end
  
(*
let ast_to_json : module_ -> Yojson.Safe.t = 
  let rec helper_e = function 
  | Program.Expr.Var s -> `Assoc [("kind", `String "Var"); "name", `String s]
  | Const n ->  `Assoc [("kind", `String "Const"); "value", `Int n]
  | Binop (op, l, r) -> 
      `Assoc  [ ("kind", `String "op"); ("name", `String op)
              ; ("left", helper_e l); ("right", helper_e r)
              ]
  in 
  let rec helper = function 
  | Program.Skip -> `String "Skip"
  | Read s -> `Assoc [("kind", `String "Read"); "name", `String s]
  | Write e  -> `Assoc [("kind", `String "Write"); "value", helper_e e]
  | Assn (l,r)  -> `Assoc [("kind", `String "Assn"); "lvalue", `String l; "rvalue", helper_e r]
  | If (cond,th,el)  -> 
    `Assoc  [ ("kind", `String "if"); "cond", helper_e cond
            ; "then", helper th; "else", helper el ]
  | While (cond,body)  -> 
      `Assoc  [ ("kind", `String "While"); "cond", helper_e cond
              ; "body", helper body ]
  | Seq (l,r)  -> 
    `Assoc  [ ("kind", `String "Seq")
            ; "left", helper l; "right", helper r ]
  | Call (name, args) -> 
    `Assoc  [ ("kind", `String "Call"); "func", `String name
            ; "args", `List (List.map helper_e args) ]
  | Fun (name, params, body) ->
    `Assoc  [ ("kind", `String "Fun"); ("name", `String name)
            ; "params", `List (List.map (fun x -> `String x) params)
            ; ("body", helper body)
            ]
  in
  fun (funs, prog) -> 
    `Assoc [ 
      ("funs", `List (List.map helper funs)); 
      ("prog", helper prog)
    ]
  


let json_to_bytecode ~fk ~fk2 : Yojson.Safe.t -> SM.t = 
  let helper = function 
  | `Int n
  | `Assoc [ ("kind", `String "Const"); ("value", `Int n)]
  | `Assoc [ ("kind", `String "CONST"); ("value", `Int n)] -> SM.CONST n
  | `Assoc [ ("kind", `String "Binop"); ("value", `String s)]
  | `Assoc [ ("kind", `String "BINOP"); ("value", `String s)] -> SM.BINOP s
  | `Assoc [ ("kind", `String "ST"); ("value", `String s)] -> SM.ST s
  | `String "READ"
  | `Assoc [ ("kind", `String "READ") ] -> SM.READ
  | `String "WRITE"
  | `Assoc [ ("kind", `String "WRITE") ] -> SM.WRITE
  | `Assoc [ ("kind", `String "JMP"); ("value", `String s)] -> SM.JMP s
  | `Assoc [ ("kind", `String "JZ"); ("value", `String s)] -> SM.JZ s
  | `Assoc [ ("kind", `String "JNZ"); ("value", `String s)] -> SM.JNZ s
  | `Assoc [ ("kind", `String "LABEL"); ("value", `String s)] -> SM.LABEL s
  (* | `String "BEGIN"
  | `Assoc [ ("kind", `String "BEGIN") ] -> SM.BEGIN *)
  | `String "END"
  | `Assoc [ ("kind", `String "END") ] -> SM.END
  | `Assoc [ ("kind", `String "Call"); ("value", `String s)]
  | `Assoc [ ("kind", `String "CALL"); ("value", `String s)] -> SM.CALL s
  | `Assoc [ ("kind", `String "Begin"); ("value", `List args)]
  | `Assoc [ ("kind", `String "BEGIN"); ("value", `List args)] -> 
      let args = List.map (function `String s -> s 
      | _ -> failwith "Bad arg in BEGIN") args
      in
      SM.BEGIN args
  | `String s
  | `Assoc [ ("kind", `String "LD"); ("value", `String s)]
  | `Assoc [ ("kind", `String "Load"); ("value", `String s)] -> SM.LD s
  | _ -> fk "неразобранный случай"
  in
  function 
  | `List xs -> List.map helper xs
  | _ -> fk2 "ожидался список"
    
let __ () = 
  let input = {|  
  read(n);
  fac:=1;
  while (n>1) do 
    fac := fac * n;
    n := n - 1
  od;
  write(fac)
|} in 
  match Parser.parse input with 
  | `Ok _ -> print_endline "OK"
  | `Fail msg -> print_endline msg

let __ () = 
  let input = {|  
    fun fact (n) {
      if n <= 1 then f := 1 else fact (n-1); f := f * n fi
   }
   
   fact (5);
   write (f)
  |} in 
  match Parser.parse input with 
  | `Ok _ -> print_endline "OK"
  | `Fail msg -> print_endline msg

let _ = 
  let j  =`List
  [
    (* main *)
    `String "READ";
    `Assoc [ ("kind", `String "CALL"); ("value", `String "fact") ];
    `String "END";
    (* function helper *)
    `Assoc [ ("kind", `String "LABEL"); ("value", `String "helper") ];
    `Assoc
      [
        ("kind", `String "BEGIN");
        ("value", `List [ `String "n"; `String "acc" ]);
      ];
    (* n == 1 *)
    `Assoc [ ("kind", `String "Load"); ("value", `String "n") ];
    `Int 1;
    `Assoc [ ("kind", `String "Binop"); ("value", `String "==") ];
    `Assoc [ ("kind", `String "JZ"); ("value", `String "helper_else") ];
    `Assoc [ ("kind", `String "Load"); ("value", `String "acc") ];
    `String "WRITE";
    `Assoc [ ("kind", `String "JMP"); ("value", `String "helper_fin") ];
    `Assoc [ ("kind", `String "LABEL"); ("value", `String "helper_else") ];
    (* acc*n *)
    `Assoc [ ("kind", `String "Load"); ("value", `String "acc") ];
    `Assoc [ ("kind", `String "Load"); ("value", `String "n") ];
    `Assoc [ ("kind", `String "Binop"); ("value", `String "*") ];
    (* n-1 *)
    `Assoc [ ("kind", `String "Load"); ("value", `String "n") ];
    `Int 1;
    `Assoc [ ("kind", `String "Binop"); ("value", `String "-") ];

    `Assoc [ ("kind", `String "CALL"); ("value", `String "helper") ];
    `Assoc [ ("kind", `String "LABEL"); ("value", `String "helper_fin") ];
    `String "END";
    (* function fact *)
    `Assoc [ ("kind", `String "LABEL"); ("value", `String "fact") ];
    `Assoc [ ("kind", `String "BEGIN"); ("value", `List [ `String "n" ]) ];
    `Int 1;
    `Assoc [ ("kind", `String "Load"); ("value", `String "n") ];
    `Assoc [ ("kind", `String "CALL"); ("value", `String "helper") ];
    `String "END";
  ]
in 
match json_to_bytecode ~fk:(fun _ -> assert false) ~fk2:(fun _ -> assert false) j with 
| bc -> Format.printf "%a\n%!" (Format.pp_print_list Format.pp_print_int) (SM.eval [3] bc ) *)
