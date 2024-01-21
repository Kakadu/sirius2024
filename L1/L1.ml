open GT

module Algebra =
  struct
  
    @type value = int with show

    let if_bool = function
    | 0 -> false
    | 1 -> true
    | x -> failwith (Printf.sprintf "non-boolean value \"%d\"" x)
       
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

    module Expr =
      struct
      
        @type t =
        | Var   of string
        | Const of int           
        | Binop of string * t * t
        with show
                    
        let rec eval st = function
        | Var   x -> st x
        | Const n -> n
        | Binop (op, l, r) -> (Algebra.evalOp op) (eval st l) (eval st r)

      end

    @type t =
    | Skip
    | Read   of string
    | Write  of Expr.t
    | Assn   of string * Expr.t
    | If     of Expr.t * t * t
    | While  of Expr.t * t
    | Seq    of t * t
    with show
    
    let empty  x        = failwith (Printf.sprintf "undefined variable \"%s\"" x)
    let update st x n y = if y = x then n else st y

    let eval i p =
      let rec eval ((st, i, o) as c) = function
        | Skip  -> c
        
        | Read x ->
          (match i with
           | n :: i' -> update st x n, i', o
           | _       -> failwith "input stream is exhausted"
          )
      
        | Write e ->
          st, i, Expr.eval st e :: o
             
        | Assn (x, e) ->
          update st x (Expr.eval st e), i, o
          
        | If (f, t, e) ->
          eval c @@
          if Algebra.if_bool (Expr.eval st f)
          then t
          else e
            
        | While (f, s) as w ->
          if Algebra.if_bool (Expr.eval st f)
          then eval (eval c s) w
          else c
            
        | Seq (s1, s2)  ->
          eval (eval c s1) s2
      in
      let _, _, o = eval (empty, i, []) p in
      List.rev o
                           
  end

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
      let rec eval ((st, s, i, o) as c) = function
        | [] -> c
        | LD    x  :: tl -> eval (st, st x :: s, i, o) tl
        | CONST n  :: tl -> eval (st, n    :: s, i, o) tl
        | BINOP op :: tl ->
          (match s with
           | x :: y :: s' -> eval (st, Algebra.evalOp op y x :: s', i, o) tl
           | _            ->
             failwith (Printf.sprintf "exhausted stack at BINOP %s: \"%s\"" op ((show(list) (show(int))) s))
          )
        | ST x :: tl ->
          (match s with
           | n :: s' -> eval (Program.update st x n, s', i, o) tl
           | _       -> failwith (Printf.sprintf "exhausted stack at ST %s" x)
          )
        | READ :: tl ->
          (match i with
           | n :: i' -> eval (st, n :: s, i', o) tl
           | _       -> failwith "exhausted input stream"
          )
        | WRITE :: tl ->
          (match s with
           | n :: s' -> eval (st, s', i, n :: o) tl
           | _       -> failwith "exhausted stack at WRITE"
          )
        | JMP l :: _ ->
          eval c (lookup l)
        | JZ l :: tl ->
          (match s with
           | n :: s' ->
             eval (st, s', i, o) @@
             if Algebra.if_bool n
             then tl
             else lookup l
           | _ -> failwith (Printf.sprintf "exhausted stack at JZ %s" l)
          )
        | JNZ l :: tl ->
          (match s with
           | n :: s' ->
             eval (st, s', i, o) @@
             if Algebra.if_bool n
             then lookup l
             else tl
           | _ -> failwith (Printf.sprintf "exhausted stack at JZ %s" l)
          )          
        | LABEL _ :: tl -> eval c tl
      in
      let _, _, _, o = eval (Program.empty, [], i, []) p in
      List.rev o
      
  end
  
module Parser =
  struct

    open Ostap
    open Ostap.Util

    let expression primary =
      let binop op x y = Program.Expr.Binop (op, x, y) in
      expr
        (fun x -> x)
        [|
          (* Disjunction *)
          `Lefta, [ostap ("|"), binop "|"];

          (* Conjunction *)
          `Lefta, [ostap ("&"), binop "&"];

          (* Relational operators *)
          `Nona, [ostap ("<" ), binop "<";
                  ostap ("<="), binop "<=";
                  ostap (">" ), binop ">";
                  ostap (">="), binop ">=";
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
      primary:
        x:DECIMAL {Program.Expr.Const x}
      | x:LIDENT  {Program.Expr.Var x}
      | -"(" expr -")";
      
      expr: expression[primary];

      simple_stmt:
        x:LIDENT ":=" e:expr {Program.Assn (x, e)}
      | "skip"               {Program.Skip}
      | "if" c:expr
        "then" t:stmt
        "else" e:stmt
        "fi"                 {Program.If (c, t, e)}
      | "while" c:expr
        "do" s:stmt "od"     {Program.While (c, s)}
      | "read"
        "(" x:LIDENT ")"     {Program.Read (x)}
      | "write"
        "(" e:expr ")"       {Program.Write (e)};

      stmt: h:simple_stmt t:(-";" stmt)? {
        match t with
        | None   -> h
        | Some t -> Program.Seq (h, t)
      };

      input: !(Util.list)[ostap (DECIMAL)]
    )

    let parse_input =
      let kws = [] in
      fun s ->
        parse
          (object
            inherit Matcher.t s
            inherit Util.Lexers.decimal s
            inherit Util.Lexers.skip [
                Matcher.Skip.whitespaces " \t\n\r";
                Matcher.Skip.lineComment "--";
                Matcher.Skip.nestedComment "(*" "*)"
              ] s
            inherit Util.Lexers.lident kws s
          end
          )
          (ostap (input -EOF))

    let parse =
      let kws = ["skip"; "if"; "fi"; "then"; "else"; "do"; "od"; "while"; "read"; "write"] in
      fun s ->
        parse
          (object
            inherit Matcher.t s
            inherit Util.Lexers.decimal s
            inherit Util.Lexers.skip [
                Matcher.Skip.whitespaces " \t\n\r";
                Matcher.Skip.lineComment "--";
                Matcher.Skip.nestedComment "(*" "*)"
              ] s
            inherit Util.Lexers.lident kws s
          end
          )
          (ostap (stmt -EOF))
 
  end

let ast_to_json =
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
  in
  helper


let json_to_bytecode ~fk ~fk2 : Yojson.Safe.t -> SM.t = 
  let rec helper = function 
  | `Int n
  | `Assoc [ ("kind", `String "Const"); ("value", `Int n)] -> SM.CONST n
  | `Assoc [ ("kind", `String "Binop"); ("value", `String s)] -> SM.BINOP s
  | `Assoc [ ("kind", `String "ST"); ("value", `String s)] -> SM.ST s
  | `String "READ"
  | `Assoc [ ("kind", `String "READ") ] -> SM.READ
  | `String "WRITE"
  | `Assoc [ ("kind", `String "WRITE") ] -> SM.WRITE
  | `String s
  | `Assoc [ ("kind", `String "LD"); ("value", `String s)]
  | `Assoc [ ("kind", `String "Load"); ("value", `String s)] -> SM.LD s
  | `Assoc [ ("kind", `String "JMP"); ("value", `String s)] -> SM.JMP s
  | `Assoc [ ("kind", `String "JZ"); ("value", `String s)] -> SM.JZ s
  | `Assoc [ ("kind", `String "JNZ"); ("value", `String s)] -> SM.JNZ s
  | `Assoc [ ("kind", `String "LABEL"); ("value", `String s)] -> SM.LABEL s
  | _ -> fk "неразобранный случай"
  in
  function 
  | `List xs -> List.map helper xs
  | _ -> fk2 "ожидался список"


let parse_input _ = `Ok [1;2;3]

let () =
  let test_input input = 
    Printf.printf "Input: %S\n%!" input;
    match Parser.parse_input input with 
    | `Ok _ -> print_endline "OK"
    | `Fail s -> print_endline s
  in 
  test_input "1";
  test_input "1 2"