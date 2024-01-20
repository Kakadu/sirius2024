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