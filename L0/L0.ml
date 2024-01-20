open GT

module Algebra =
  struct
  
    @type value = int with show
       
    let evalOp =
      let of_bool f x y = if f x y then 1 else 0 in
      let if_bool = function
      | 0 -> false
      | 1 -> true
      | x -> failwith (Printf.sprintf "non-boolean value \"%d\"" x)
      in
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
    | Var   of string
    | Const of int           
    | Binop of string * t * t
    with show

    let empty x = failwith (Printf.sprintf "undefined variable \"%s\"" x)
    let update st x n y = if y = x then n else st y

    let rec eval st = function
    | Var   x -> st x
    | Const n -> n
    | Binop (op, l, r) -> (Algebra.evalOp op) (eval st l) (eval st r)
                            
  end

module SM =
  struct

    @type insn =
    | CONST of int
    | LD    of string       
    | BINOP of string
    with show

    @type t = insn list with show

    let compile p =
      let rec compile acc = function
      | Program.Var   x          -> LD x :: acc
      | Program.Const n          -> CONST n :: acc                       
      | Program.Binop (op, l, r) -> compile (compile (BINOP op :: acc) r) l
      in
      List.rev @@ compile [] p

    let rec eval st s = function
    | [] -> (match s with
             | [x] -> x
             | _   -> failwith (Printf.sprintf "non-unit stack at the end of execution: \"%s\"" @@ (show(list) (show(int))) s)
            )
    | LD    x  :: tl -> eval st (st x :: s) tl
    | CONST n  :: tl -> eval st (n    :: s) tl
    | BINOP op :: tl ->
      (match s with
       | x :: y :: s' -> eval st (Algebra.evalOp op y x :: s') tl
       | _            ->
         failwith (Printf.sprintf "exhausted stack at BINOP %s: \"%s\"" op ((show(list) (show(int))) s))
      )
      
  end
  
module Parser =
  struct

    open Ostap
    open Ostap.Util
    open Matcher

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
        x:DECIMAL {Program.Const x}
      | x:LIDENT  {Program.Var x}
      | -"(" expr -")";
      
      expr: expression[primary];

      state: state_chain[Program.empty];

      state_chain[st]:
        !(Combinators.empty) {st}    
      | -x:LIDENT -"=" -n:DECIMAL -"," state_chain[Program.update st x n]        
    )

    let parse =
      let kws = [] in
      fun s ->
        parse
          (object (self : 'self)
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
          (ostap (expr -EOF))
 
  end
