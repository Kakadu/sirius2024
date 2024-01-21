open Js_of_ocaml
open Js_of_ocaml.Firebug

let (_: (GT.string -> GT.int) -> L0.Program.t -> GT.int) = L0.Program.eval 

module Predefined = struct
  module Expr = struct
    let write = {| var x = 1; write(x)|}
    (* let write_bc = [%blob "1write.bc.json"] *)
  end
(* 
  module Funs = struct
    let fac = [%blob "1append.lama"]
    let fac_bc = [%blob "1append.bc.json"]
  end

  module Other = struct
    let append = [%blob "1append.lama"]
    let append_bc = [%blob "1append.bc.json"]
    let test50arrays = [%blob "test050.lama"]
    let test50arrays_bc = [%blob "test050.bc.json"]
    let test081zip = [%blob "081zip.lama"]
    let test081zip_bc = [%blob "081zip.bc.json"]
  end *)
end

(* let eval_bc_string contents =
  print_endline contents;
  let j = Yojson.Safe.from_string contents in
  let bc = LibSerialize.json_to_bytecode j in
  let rez : int list = SM.run bc [] in
  Format.printf "Result: @[%a@]\n%!" Format.(pp_print_list pp_print_int) rez;
  ()

let __ () = eval_bc_string Predefined.Other.append *)

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
    el##.textContent := Js.some @@ Js.string ("OK " ^ String.concat " " (List.map string_of_int xs))
  in
  let report_error msg = 
    let el = get_and_coerce Names.lama_output Dom_html.CoerceTo.div in
    el##.textContent := Js.some @@ Js.string (Printf.sprintf "fail: %s" msg)
  in
  
  let on_input ?(copy=false) () =
    match L0.Parser.parse (Js.to_string area##.value) with 
    | `Fail msg -> 
          report_error ("Can't parse program. " ^ msg); 
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
            report_error (Printexc.to_string exc)
        in

        let env_area = get_and_coerce Names.env Dom_html.CoerceTo.textarea in 
        let state = match L0.Parser.parse_state (Js.to_string env_area##.value) with 
        | `Fail msg -> 
            report_error ("Can't parse env. " ^ msg ^ ". Goging to use default one"); 
            (function _ -> 42)
        | `Ok env ->
            report_success []; env
        in 
        
        let () = 
          try 
            let rez = L0.Program.eval state ast  in 
            log "rez = %d, copy = %b\n" rez copy; 
            report_success [rez];
          with exc -> report_error (Printexc.to_string exc)
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
    match LibSerialize.json_to_bytecode (Yojson.Safe.from_string (Js.to_string area##.value)) with 
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
        status##.textContent := Js.some (Js.string ("Can't parse env. " ^ msg))
    | `Ok env ->
        status##.textContent := Js.some (Js.string "OK");
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
(*
module Eval_bc_gui = struct 
  let clear_output_area () =
    (Dom_html.getElementById_exn "output_area")##.textContent := Js.null

  let get_stru_text_exn () =
    Dom_html.getElementById_coerce "input_area" Dom_html.CoerceTo.textarea
    |> Option.get

  let run_clicked () =
    clear_output_area ();
    let textarea = get_stru_text_exn () in
    (* (Dom_html.getElementById_exn "output_area")##.textContent
      := Js.some textarea##.value *)
    let j = Yojson.Safe.from_string (Js.to_string textarea##.value) in
    let bc = LibSerialize.json_to_bytecode j in
    Printf.printf "%s %d\n" __FILE__ __LINE__;
    let rez : int = L0.SM.eval (fun s -> failwith "WTF") [] bc in
    (Dom_html.getElementById_exn "output_area")##.textContent
    := Js.some
      @@ Js.string
            (Format.asprintf "Result: @[%a@]\n%!"
              Format.pp_print_int
              rez)

  let () =
    (Dom_html.getElementById_exn "send1")##.onclick
    := Dom.handler (fun _ ->
          run_clicked ();
          Js._true)

  let () =
    let input_area = Dom_html.createTextarea Dom_html.document in
    input_area##setAttribute (Js.string "id") (Js.string "input_area");
    Dom.appendChild (Dom_html.getElementById_exn "left") input_area

  let () =
    let examples =
      [
         ("write", {|x+1 |});
        (*("append", Predefined.Other.append_bc);
        ("test50array", Predefined.Other.test50arrays_bc);
        ("write", Predefined.Expr.write_bc); *)
      ]
    in
    let _combo =
      Dom_html.getElementById_coerce "demos" Dom_html.CoerceTo.select
      |> Option.get
    in
    List.iter
      (fun (name, _) ->
        _combo##add
          (let g = Dom_html.createOption Dom_html.document in
            g##.label := Js.string name;
            g)
          Js.null)
      examples;
    let on_change () =
      console##log _combo##.selectedIndex;
      let textarea = get_stru_text_exn () in
  
      textarea##.value :=
        Js.string (snd (List.nth examples _combo##.selectedIndex))
    in
    _combo##.onchange :=
      Dom_html.handler (fun _ ->
          on_change ();
          Js._true);
    _combo##.selectedIndex := 0;
    on_change ()
end *)

(*
module Lama2JSON = struct
  let () =
    let input_area = Dom_html.createTextarea Dom_html.document in
    input_area##setAttribute (Js.string "id") (Js.string "input_area_lama2json");
    Dom.appendChild (Dom_html.getElementById_exn "left_lama2json") input_area

  (* let () =
    (Dom_html.getElementById_exn "left_lama2json")##.onclick
    := Dom.handler (fun _ ->
           console##log (Js.string "parse lama here");
           Js._true) *)


  let waiting_output () =
    let out_container = Dom_html.getElementById_exn "output_lama2json" in 
    out_container##.textContent := Js.some (Js.string "waiting...")
          
  let set_output text ast =
    let out_container = (Dom_html.getElementById_exn "output_lama2json") in 
    out_container##.textContent := Js.null;
    let pre =  Dom_html.createPre Dom_html.document in 
    pre##.textContent := Js.some (Js.string text);
    Dom.appendChild out_container pre;
    print_endline "set_output finished"        
  let () =
    let examples =
      [
        ("test1", {|1+1 |});
        ("test2", {|1+2*3 |});
        (* ("zip", Predefined.Other.test081zip);
        ("append", Predefined.Other.append);
        ("test50array", Predefined.Other.test50arrays);
        ("write", Predefined.Expr.write); *)
      ]
    in
    let _combo =
      Dom_html.getElementById_coerce "lamaDemos" Dom_html.CoerceTo.select
      |> Option.get
    in
    List.iter
      (fun (name, code_) ->
        _combo##add
          (let g = Dom_html.createOption Dom_html.document in
           g##.label := Js.string name;
           Printf.printf "option %S created with value %S\n%!" name code_;
           g)
          Js.null)
      examples;
    let on_change () =
      console##log _combo##.selectedIndex;
      let textarea =
        Dom_html.getElementById_coerce "input_area_lama2json"
          Dom_html.CoerceTo.textarea
        |> Option.get
      in

      textarea##.value :=
        Js.string (snd (List.nth examples _combo##.selectedIndex))
    in

    _combo##.onchange :=
      Dom_html.handler (fun _ ->
          on_change ();
          Js._true);
    _combo##.selectedIndex := 0;
    (* on_change (); *)
    ()

  let () =
    (Dom_html.getElementById_exn "lamaToJsonBtn")##.onclick
    := Dom.handler (fun _ ->
           console##log (Js.string "parsing...");
           let contents =
             let textarea =
               Dom_html.getElementById_coerce "input_area_lama2json"
                 Dom_html.CoerceTo.textarea
               |> Option.get
             in
             Js.to_string textarea##.value
           in

           (* let contents = In_channel.with_open_text name In_channel.input_all in *)
           let ast =
             (* let name = "asdf.lama" in *)
             waiting_output ();
             let rez = L0.Parser.parse contents in
             match rez with
             | `Fail s ->
                 Printf.eprintf "Lama Parsing error:\n%s\n%!" s;
                 exit 1
             | `Ok prog -> 
              set_output "parsed" rez;
              prog
           in
           Js._true)
end
*)