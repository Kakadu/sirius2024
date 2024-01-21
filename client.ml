open Js_of_ocaml
open Js_of_ocaml.Firebug

let log fmt =
  if true then
    Format.kfprintf
      (fun out -> Format.fprintf out "\n%!")
      Format.std_formatter fmt
  else Format.ikfprintf (fun _ppf -> ()) Format.std_formatter fmt

let failwiths fmt = Printf.kprintf failwith fmt

module Lang = L1

module Names = struct
  let env = "env-area"
  let env_status = "env-area-status"
  let lama_src = "lama-src-area"
  let lama_output = "lama-program-output"
  let lama_json_area = "src-json-area"
  let bytecode_src = "bc-json-area"
  let bytecode_output = "bc-program-ouput"
  let compileLamaBtn = "compileLamaBtn"
  let runBcBtn = "runBcBtn"
end

let get_and_coerce name dest =
  match Dom_html.getElementById_coerce name dest with
  | None -> failwiths "Can get element %S" name
  | Some x -> x

let pp_int_list ppf xs =
  (Format.pp_print_list
     ~pp_sep:(fun ppf () -> Format.pp_print_space ppf ())
     Format.pp_print_int)
    ppf xs

let parse_input s = Lang.Parser.parse_input s

(* LAMA *)
let on_lama_changed =
  let area = get_and_coerce Names.lama_src Dom_html.CoerceTo.textarea in
  let report_success xs =
    let el = get_and_coerce Names.lama_output Dom_html.CoerceTo.div in
    el##.style##.color := Js.string "color: black;";
    el##.textContent :=
      Js.some
      @@ Js.string ("OK " ^ String.concat " " (List.map string_of_int xs))
  in
  let report_lama_error msg =
    print_endline "report lama error";
    let el = get_and_coerce Names.lama_output Dom_html.CoerceTo.div in
    el##.style##.color := Js.string "color: red;";
    el##.textContent := Js.some @@ Js.string (Printf.sprintf "fail: %s" msg)
  in

  let on_input ?(copy = false) () =
    match Lang.Parser.parse (Js.to_string area##.value) with
    | `Fail msg ->
        report_lama_error ("Can't parse program. " ^ msg);
        (get_and_coerce Names.lama_json_area Dom_html.CoerceTo.pre)##.textContent
        := Js.null
    | `Ok ast ->
        let () =
          let area =
            get_and_coerce Names.lama_json_area Dom_html.CoerceTo.pre
          in
          try
            let j = Lang.ast_to_json ast in
            let json_str = Js.string (Yojson.Safe.pretty_to_string j) in
            area##.textContent := Js.some json_str;
            let _ =
              (Js.Unsafe.eval_string
                 {|text => navigator.clipboard.writeText(text); |}
                : Js.js_string Js.t -> unit)
                json_str
            in
            ()
          with exc -> report_lama_error (Printexc.to_string exc)
        in

        let env_area = get_and_coerce Names.env Dom_html.CoerceTo.textarea in
        let state =
          match parse_input (Js.to_string env_area##.value) with
          | `Fail msg ->
              report_lama_error
                ("Can't parse env. " ^ msg ^ ". Going to use default one");
              []
          | `Ok env ->
              log "Environment: %a" pp_int_list env;
              report_success [];
              env
        in

        let () =
          try
            let rez = Lang.Program.eval state ast in
            log "rez = %a, copy = %b\n" pp_int_list rez copy;
            report_success rez
          with exc -> report_lama_error (Printexc.to_string exc)
        in
        let () =
          if copy then
            ignore
            @@ Js.Unsafe.eval_string
                 {| 
            var snackbarContainer = document.querySelector('#demo-snackbar-example');
            var data = {
              message: 'JSON в буфере',
              timeout: 2000,
              //actionHandler: handler,
              //actionText: 'Undo'
            };
            snackbarContainer.MaterialSnackbar.showSnackbar(data);
            |}
          else ()
        in
        ()
  in
  area##.oninput :=
    Dom.handler (fun _ ->
        on_input ~copy:true ();
        Js._true);
  (get_and_coerce Names.compileLamaBtn Dom_html.CoerceTo.button)##.onclick
  := Dom.handler (fun _ ->
         on_input ~copy:true ();
         Js._true);
  on_input

exception Bad_JSON_for_bytecode of string

(* Bytecode *)
let on_bytecode_changed : unit -> unit =
  let area = get_and_coerce Names.bytecode_src Dom_html.CoerceTo.textarea in
  let report_success xs =
    let el = get_and_coerce Names.bytecode_output Dom_html.CoerceTo.pre in
    el##.textContent :=
      Js.some
      @@ Js.string ("OK " ^ String.concat " " (List.map string_of_int xs))
  in
  let report_error msg =
    let el = get_and_coerce Names.bytecode_output Dom_html.CoerceTo.pre in
    el##.textContent := Js.some @@ Js.string (Printf.sprintf "fail: %s" msg)
  in

  let on_input () =
    (* print_endline "on_bytecode_changed"; *)
    match Yojson.Safe.from_string (Js.to_string area##.value) with
    | exception exc ->
        let msg = Printexc.to_string exc in
        console##error (Js.string msg);
        report_error ("Ошибка в JSON.\n" ^ msg)
    | json -> (
        let fk s = raise (Bad_JSON_for_bytecode s) in
        match Lang.json_to_bytecode ~fk2:fk ~fk json with
        | exception Bad_JSON_for_bytecode msg ->
            report_error ("Can't parse bytecode program. " ^ msg)
        | bc ->
            let env_area =
              get_and_coerce Names.env Dom_html.CoerceTo.textarea
            in
            let state : int list =
              match parse_input (Js.to_string env_area##.value) with
              | `Fail msg ->
                  report_error
                    ("Can't parse env. " ^ msg ^ ". Goging to use default one");
                  []
              | `Ok env ->
                  report_success [];
                  env
            in

            let () =
              try
                let rez = Lang.SM.eval state bc in
                log "rez = %a\n" pp_int_list rez;
                report_success rez
              with exc -> report_error (Printexc.to_string exc)
            in
            ())
  in
  area##.oninput :=
    Dom.handler (fun _ ->
        on_input ();
        Js._true);
  (get_and_coerce Names.runBcBtn Dom_html.CoerceTo.button)##.onclick
  := Dom.handler (fun _ ->
         on_input ();
         Js._true);
  on_input

(* ENV *)
let () =
  let area = get_and_coerce Names.env Dom_html.CoerceTo.textarea in
  let status = get_and_coerce Names.env_status Dom_html.CoerceTo.div in
  area##.oninput :=
    Dom.handler (fun _ ->
        (match Lang.parse_input (Js.to_string area##.value) with
        | `Fail msg ->
            status##.style##.color := Js.string "color: red;";
            status##.textContent :=
              Js.some (Js.string ("Can't parse env. " ^ msg))
        | `Ok _env ->
            status##.style##.color := Js.string "color: black;";
            status##.textContent := Js.null;
            on_lama_changed ();
            on_bytecode_changed ();
            ());
        Js._true);
  ()

let () =
  let area = get_and_coerce Names.env Dom_html.CoerceTo.textarea in
  area##.textContent := Js.some @@ Js.string {|3 2 1|}

let () =
  let area = get_and_coerce Names.lama_src Dom_html.CoerceTo.textarea in
  area##.textContent :=
    Js.some
    @@ Js.string
         {| 
  read(n);
  fac:=1;
  while (n>1) do 
    fac := fac * n;
    n := n - 1
  od;
  write(fac)
  |}

let () =
  let area = get_and_coerce Names.bytecode_src Dom_html.CoerceTo.textarea in
  let j =
    `List
      [
        `String "READ";
        `Assoc [ ("kind", `String "ST"); ("value", `String "n") ];
        `Int 1;
        `Assoc [ ("kind", `String "ST"); ("value", `String "fac") ];
        (* loop *)
        `Assoc [ ("kind", `String "LABEL"); ("value", `String "LOOP") ];
        `Assoc [ ("kind", `String "Load"); ("value", `String "n") ];
        `Int 1;
        `Assoc [ ("kind", `String "Binop"); ("value", `String ">") ];
        `Assoc [ ("kind", `String "JZ"); ("value", `String "FIN") ];
        (* fac := fac * n *)
        `Assoc [ ("kind", `String "Load"); ("value", `String "fac") ];
        `Assoc [ ("kind", `String "Load"); ("value", `String "n") ];
        `Assoc [ ("kind", `String "Binop"); ("value", `String "*") ];
        `Assoc [ ("kind", `String "ST"); ("value", `String "fac") ];
        (* n := n-1 *)
        `Assoc [ ("kind", `String "Load"); ("value", `String "n") ];
        `Int 1;
        `Assoc [ ("kind", `String "Binop"); ("value", `String "-") ];
        `Assoc [ ("kind", `String "ST"); ("value", `String "n") ];
        `Assoc [ ("kind", `String "JMP"); ("value", `String "LOOP") ];
        (* fin *)
        `Assoc [ ("kind", `String "LABEL"); ("value", `String "FIN") ];
        `Assoc [ ("kind", `String "Load"); ("value", `String "fac") ];
        `String "WRITE";
      ]
  in
  area##.textContent := Js.some @@ Js.string (Yojson.Safe.pretty_to_string j)

let () =
  on_lama_changed ();
  on_bytecode_changed ()

let () = console##log Dom_html.window##.location##.hash
