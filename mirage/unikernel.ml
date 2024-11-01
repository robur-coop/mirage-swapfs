module K = struct
  open Cmdliner

  let use_swap =
    let doc =
      Arg.info [ "use-swap" ]
    in
    Mirage_runtime.register_arg Arg.(value & flag doc)
end

module Main (B : Mirage_block.S) = struct
  open Lwt.Syntax

  module Swapfs = Swapfs.Make (B)

  let fifty_mb =
    Seq.init (50 * 1024)
      (fun _ -> String.make 1024 'E')

  let start_with_swap b =
    let* s = Swapfs.connect b in
    let h = Swapfs.empty s in
    let dispense = Seq.to_dispenser fifty_mb in
    let rec read_loop () =
      match dispense () with
      | None -> Lwt.return_unit
      | Some data ->
        let size_before = Swapfs.size h in
        let* r = Swapfs.append h data in
        Result.iter_error (fun e ->
            Logs.err (fun m -> m "Some error with swap: %a" Swapfs.pp_error e);
            exit 4)
          r;
        assert (Swapfs.size h = Int64.(add size_before (of_int (String.length data))));
        read_loop ()
    in
    let* () = read_loop () in
    let rec verify_loop offset =
      let length = min 512 Int64.(to_int (sub (Swapfs.size h) offset)) in
      if length = 0 then
        Lwt.return_unit
      else
        let* r = Swapfs.get_partial h ~offset ~length in
        match r with
        | Ok data ->
          assert (String.length data = length);
          if String.exists (function 'E' -> false | _ -> true) data then
            (Logs.err (fun m -> m "Expected all EEEs, but got something else!"); exit 4);
          verify_loop Int64.(add offset (of_int length))
        | Error e ->
          Logs.err (fun m -> m "Some error with swap: %a" Swapfs.pp_error e);
          exit 4
    in
    let* () = verify_loop 0L in
    let* () = Swapfs.free h in
    Logs.app (fun m -> m "Success!");
    Lwt.return_unit

  let start_without_swap () =
    let buf = Buffer.create 16384 in
    Seq.iter (fun data -> Buffer.add_string buf data) fifty_mb;
    if String.for_all (function 'E' -> true | _ -> false) (Buffer.contents buf) then
      Logs.app (fun m -> m "Success!")
    else
      (Logs.err (fun m -> m "Expected all EEEs, but got something else!"); exit 4);
    Lwt.return_unit

  let start b =
    if K.use_swap () then
      start_with_swap b
    else
      start_without_swap ()
end
