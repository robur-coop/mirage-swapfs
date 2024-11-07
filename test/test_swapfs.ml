Random.State.make

let seed = lazy (
  let s =
    try int_of_string (Sys.getenv "TEST_SEED")
    with _ ->
      Random.self_init ();
      Random.int 1_000_000_000
  in
  Printf.printf "random seed: $TEST_SEED = %d\n%!" s;
  s)

let rng_test_case n s f =
  Alcotest_lwt.test_case n s @@
  fun sw () ->
  f sw (Random.State.make [| Lazy.force seed |])

let random_string _g =
  "hej med dig"

module Swap = Swapfs.Make(Mem)

let alc_error = Alcotest.of_pp Swap.pp_error

open Lwt.Syntax

let make g =
  let data = random_string g in
  let blocking_factor = Random.State.int g 10240 + 1 in
  let sector_size = 512 in
  let size_sectors =
    let block_size = sector_size * blocking_factor in
    let size_blocks =
      (String.length data + pred block_size) / block_size
    in
    size_blocks * blocking_factor
  in
  let b = Mem.connect ~sector_size size_sectors in
  let+ swap = Swap.connect ~blocking_factor b in
  swap, data

let simple _sw g =
  let* swap, oracle = make g in
  let h = Swap.empty swap in
  let* r = Swap.append h oracle in
  Alcotest.(check (result unit alc_error)) "append ok" (Ok ()) r;
  let* r = Swap.get_partial h ~offset:0L ~length:(String.length oracle) in
  Alcotest.(check (result string alc_error)) "get_partial ok" (Ok oracle) r;
  Lwt.return_unit

let oracle_tests = [
  rng_test_case "simple" `Quick simple ;
]

let tests = [
  "oracle", oracle_tests ;
]

let () =
  Lwt_main.run @@
  Alcotest_lwt.run "swapfs" tests
