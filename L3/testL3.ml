open L3

let test input =
  match Parser.parse input with
  | `Fail msg -> print_endline msg
  | `Ok ast -> (
      match Program.eval [ 3 ] ast with
      | rez ->
          Format.printf "%a\n%!" (Format.pp_print_list Format.pp_print_int) rez
      | exception exc -> print_endline (Printexc.to_string exc))

let%expect_test " " =
  test
    {|
let fix = { f x -> f ({ eta -> fix(f, eta) },x) } in 
( let fac = { self n -> if n<=1 then 1 else  n*self(n-1) fi } in
write(fix(fac, 4))  
)
|};
  [%expect "L3.Program.Undefined(\"fix\")"]

let%expect_test " " =
  test [%blob "t01.l3"];
  [%expect "
    1
    5"]

let%expect_test " " =
  test [%blob "t02.l3"];
  [%expect "
    543210
    0"]

let%expect_test " " =
  test [%blob "t03.l3"];
  [%expect "120"]

let%expect_test " " =
  test [%blob "t04.l3"];
  [%expect "120"]

let%expect_test " " =
  test [%blob "t05.l3"];
  [%expect "
    1111
    12"]

let%expect_test " " =
  test [%blob "t06.l3"];
  [%expect "120"]
