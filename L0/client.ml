open Js_of_ocaml
open Js_of_ocaml.Firebug

let () = 
  let open L0 in 
  let ast = 
    match Parser.parse {| 1+x|} with 
    | `Fail _ -> assert false 
    | `Ok ast -> ast
  in 
  print_endline "Parsed";
  let bc = SM.compile ast in 
  Printf.printf "%s\n%!" (GT.show SM.t bc)

let log fmt = 
  Format.ksprintf (Format.printf "%s\n%!") fmt 
let failwiths fmt = Printf.kprintf failwith fmt

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
  match  Dom_html.getElementById_coerce name dest with 
  | None -> failwiths "Can get element %S" name
  | Some x -> x 

(* LAMA *)
let on_lama_changed = 
  let area = get_and_coerce Names.lama_src Dom_html.CoerceTo.textarea in
  let report_success xs = 
    let el = get_and_coerce Names.lama_output Dom_html.CoerceTo.div in
    el##.style##.color := (Js.string "color: black;"); 
    el##.textContent := Js.some @@ Js.string ("OK " ^ String.concat " " (List.map string_of_int xs))
  in
  let report_lama_error msg = 
    print_endline "report lama error";
    let el = get_and_coerce Names.lama_output Dom_html.CoerceTo.div in
    el##.style##.color := (Js.string "color: red;"); 
    el##.textContent := Js.some @@ Js.string (Printf.sprintf "fail: %s" msg)
  in
  
  let on_input ?(copy=false) () =
    match L0.Parser.parse (Js.to_string area##.value) with 
    | `Fail msg -> 
          report_lama_error ("Can't parse program. " ^ msg); 
          (get_and_coerce Names.lama_json_area Dom_html.CoerceTo.pre)##.textContent := Js.null
    | `Ok ast -> (
        let () = 
          let area = get_and_coerce Names.lama_json_area Dom_html.CoerceTo.pre in 
          try 
            let j = LibSerialize.lama_to_json ast in
            let json_str = Js.string (Yojson.Safe.pretty_to_string j) in
            area##.textContent := Js.some json_str;
            let _ = 
              (Js.Unsafe.eval_string {|text => navigator.clipboard.writeText(text); |} : Js.js_string Js.t -> unit) 
              json_str
            in 
            ()
          with exc -> 
            report_lama_error (Printexc.to_string exc)
        in

        let env_area = get_and_coerce Names.env Dom_html.CoerceTo.textarea in 
        let state = match L0.Parser.parse_state (Js.to_string env_area##.value) with 
        | `Fail msg -> 
            report_lama_error ("Can't parse env. " ^ msg ^ ". Going to use default one"); 
            (function _ -> 42)
        | `Ok env ->
            report_success []; env
        in 
        
        let () = 
          try 
            let rez = L0.Program.eval state ast  in 
            log "rez = %d, copy = %b\n" rez copy; 
            report_success [rez];
          with exc -> report_lama_error (Printexc.to_string exc)
        in
        let () = 
          if copy then 
            ignore @@ Js.Unsafe.eval_string {| 
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
        ())
    in 
    area##.oninput := Dom.handler (fun _ ->
      on_input ~copy:true (); 
      Js._true);
    (get_and_coerce Names.compileLamaBtn Dom_html.CoerceTo.button)##.onclick := 
      Dom.handler (fun _ -> on_input ~copy:true (); Js._true);
    on_input

(* Bytecode *)
let on_bytecode_changed : unit -> unit = 
  let area = get_and_coerce Names.bytecode_src Dom_html.CoerceTo.textarea in
  let report_success xs = 
    let el = get_and_coerce Names.bytecode_output Dom_html.CoerceTo.pre in
    el##.textContent := Js.some @@ Js.string ("OK " ^ String.concat " " (List.map string_of_int xs))
  in
  let report_error msg = 
    let el = get_and_coerce Names.bytecode_output Dom_html.CoerceTo.pre in
    el##.textContent := Js.some @@ Js.string (Printf.sprintf "fail: %s" msg)
  in

  let on_input () = 
    print_endline "on_bytecode_changed";
    match (Yojson.Safe.from_string (Js.to_string area##.value)) with 
    | exception exc -> 
      let msg = (Printexc.to_string exc) in 
      console##error (Js.string msg);
      report_error ("Ошибка в JSON.\n" ^ msg)
    | json -> 
      match LibSerialize.json_to_bytecode json with 
      | exception (LibSerialize.Bad_JSON_for_bytecode msg) -> 
            report_error ("Can't parse bytecode program. " ^ msg)
      | bc -> (
          let env_area = get_and_coerce Names.env Dom_html.CoerceTo.textarea in 
          let state : string -> int = match L0.Parser.parse_state (Js.to_string env_area##.value) with 
          | `Fail msg -> 
              report_error ("Can't parse env. " ^ msg ^ ". Goging to use default one"); 
              (function _ -> 42)
          | `Ok env ->
              report_success []; env
          in 
        
          let () = 
            try 
              let rez = L0.SM.eval state [] bc in 
              log "rez = %d\n" rez; 
              report_success [rez];
            with exc -> report_error (Printexc.to_string exc)
          in
          ())
      in
  area##.oninput := Dom.handler (fun _ -> on_input (); Js._true);
  (get_and_coerce Names.runBcBtn Dom_html.CoerceTo.button)##.onclick := 
    Dom.handler (fun _ -> on_input (); Js._true);
  on_input
  
(* ENV *)
let () = 
  let area = get_and_coerce Names.env Dom_html.CoerceTo.textarea in
  let status = get_and_coerce Names.env_status Dom_html.CoerceTo.div in
  area##.oninput := Dom.handler (fun _ ->
    (match L0.Parser.parse_state (Js.to_string area##.value) with 
    | `Fail msg -> 
        status##.style##.color := (Js.string "color: red;"); 
        status##.textContent := Js.some (Js.string ("Can't parse env. " ^ msg))
    | `Ok env ->
        status##.style##.color := (Js.string "color: black;"); 
        status##.textContent := Js.null;
        on_lama_changed();
        on_bytecode_changed();
        ()
        );
    Js._true);
  ()

let () = 
  let area = get_and_coerce Names.env Dom_html.CoerceTo.textarea in
  area##.textContent := Js.some @@ Js.string {|z=3 x=1|}

let () = 
  let area = get_and_coerce Names.lama_src Dom_html.CoerceTo.textarea in
  area##.textContent := Js.some @@ Js.string "x+z+0"

let () = 
  let area = get_and_coerce Names.bytecode_src Dom_html.CoerceTo.textarea in
  let j = `List [ `String "x"; `String "z"; `Assoc [ ("kind", `String "Binop"); ("value", `String "+")] ] in 
  area##.textContent := Js.some @@ Js.string (Yojson.Safe.pretty_to_string j)

let () = 
      on_lama_changed ();
      on_bytecode_changed ()
