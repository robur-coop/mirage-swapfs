(* mirage >= 4.8.0 & < 4.9.0 *)

open Mirage

let app =
  let pin = "git+file://" ^ Filename.dirname (Sys.getcwd ()) ^ "#HEAD" in
  let packages = [
    package ~pin "swapfs";
  ] in
  main ~packages "Unikernel.Main"
    (block @-> job)

let () =
  register "swappy"
    [ app $ block_of_file "swap" ]
