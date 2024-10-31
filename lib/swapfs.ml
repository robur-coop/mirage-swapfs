module type S = sig
  type t

  type handle

  type error

  val pp_error : Format.formatter -> error -> unit

  val empty : t -> handle

  val append : handle -> string -> (unit, error) result Lwt.t

  val size : handle -> int64

  val get_partial : handle -> offset:int64 -> length:int -> (string, error) result Lwt.t

  val free : handle -> unit Lwt.t
end

module Make (B : Mirage_block.S) = struct
  (* TODO? Keep < sector_size slack in memory *)

  type t = {
    b : B.t;
    sector_size : int;
    allocations : handle Weak.t;
    blocking_factor : int;
  }

  and handle = {
    mutable length : int64;
    mutable blocks : int64 list;
    mutex : Lwt_mutex.t;
    t : t;
  }

  type error = [
    | `Block of B.error
    | `Block_wr of B.write_error
    | `Out_of_space
  ]

  let pp_error ppf = function
    | _ -> Format.pp_print_string ppf "TODO: some error message" (* FIXME *)

  open Lwt.Syntax

  let ( let*? ) = Lwt_result.bind

  let write b sector_start buffers =
    let+ r = B.write b sector_start buffers in
    Result.map_error (fun e -> `Block_wr e) r

  let read b sector_start buffers =
    let+ r = B.read b sector_start buffers in
    Result.map_error (fun e -> `Block e) r

  let empty t =
    let handle = {
      length = 0L;
      blocks = [];
      mutex = Lwt_mutex.create ();
      t;
    } in
    handle

  let free_blocks t blocks =
    List.iter
      (fun i ->
         Weak.set t.allocations (Int64.to_int i) None)
      blocks

  let free handle =
    Lwt_mutex.with_lock handle.mutex @@ fun () ->
    free_blocks handle.t handle.blocks;
    handle.length <- 0L;
    handle.blocks <- [];
    Lwt.return_unit

  let size handle = handle.length

  let alloc handle n =
    let remaining = ref n in
    let res = ref [] in
    let exception Done of int64 list in
    try
      for i = 0 to Weak.length handle.t.allocations - 1 do
        if !remaining = 0 then
          raise_notrace (Done !res);
        if not (Weak.check handle.t.allocations i) then begin
          decr remaining; res := Int64.of_int i :: !res
        end
      done;
      Error `Out_of_space
    with Done res -> Ok res

  let sectors_of_block blocking_factor block =
    Seq.init blocking_factor
      (fun i -> Int64.(add block (of_int i)))

  let rec sectors_of_blocks blocking_factor sector_offset blocks =
    match sector_offset, blocks with
    | _, [] -> Seq.empty
    | 0, block :: blocks ->
      Seq.append (sectors_of_block blocking_factor block)
        (sectors_of_blocks blocking_factor 0 blocks)
    | n, block :: blocks ->
      Seq.drop n
        (Seq.append (sectors_of_block blocking_factor block)
           (sectors_of_blocks blocking_factor 0 blocks))

  let get_partial handle ~offset ~length =
    if length < 0 then
      invalid_arg "negative length";
    if offset < 0L then
      invalid_arg "negative offset";
    if Int64.(add offset (of_int length)) < handle.length then
      invalid_arg "out of bounds";
    let res = Bytes.create length in
    let blocks = List.rev handle.blocks in
    let block_size = Int64.(mul (of_int handle.t.blocking_factor)
                              (of_int handle.t.sector_size))
    in
    let start_block = Int64.(to_int (div offset block_size)) in
    let blocks =
      let rec drop n xs =
        if n <= 0 then
          xs
        else
          match xs with
          | [] as rest | _ :: rest -> drop (pred n) rest
      in
      drop start_block blocks
    in
    let pre_slack = Int64.(to_int (rem offset block_size)) in
    let scratch = Cstruct.create handle.t.sector_size in
    let blocks =
      sectors_of_blocks handle.t.blocking_factor
        (pre_slack / handle.t.sector_size)
        blocks
    in
    let disp = Seq.to_dispenser blocks in
    let rec loop off dest_off length =
      if length = 0 then
        Lwt_result.return ()
      else
        (* NOTE: should always be [Some _] due to invariants *)
        let sector = Option.get (disp ()) in
        let*? () = read handle.t.b sector [ scratch ] in
        let l = min handle.t.sector_size (length - off) in
        Cstruct.blit_to_bytes scratch off res dest_off l;
        loop 0 (dest_off + l) (length - l)
    in
    let*? () = loop (pre_slack mod handle.t.sector_size) 0 length in
    Lwt_result.return (Bytes.unsafe_to_string res)

  let append handle data =
    Lwt_mutex.with_lock handle.mutex @@ fun () ->
    let block_size = Int64.(mul (of_int handle.t.blocking_factor)
                              (of_int handle.t.sector_size)) in
    let cur_slack = Int64.(rem handle.length block_size) in
    let*? new_blocks =
      alloc handle 
        Int64.(to_int (div (add cur_slack (of_int (String.length data)))
                         block_size))
      |> Lwt.return
    in
    let touched = 
      if cur_slack > 0L then
        List.hd handle.blocks :: new_blocks
      else
        new_blocks
    in
    let disp =
      sectors_of_blocks handle.t.blocking_factor
        Int64.(to_int (div cur_slack (of_int handle.t.sector_size)))
        touched
      |> Seq.to_dispenser
    in
    let scratch = Cstruct.create handle.t.sector_size in
    let rec loop off src_off length =
      if length = 0 then
        Lwt_result.return ()
      else
        (* NOTE: should always be [Some _] due to invariants *)
        let sector = Option.get (disp ()) in
        let*? () =
          if off > 0 then
            read handle.t.b sector [ scratch ]
          else Lwt_result.return ()
        in
        let l = min (handle.t.sector_size - off) (length) in
        Cstruct.blit_from_string data src_off scratch off l;
        let*? () = write handle.t.b sector [ scratch ] in
        loop 0 (src_off + l) (length - l)
    in
    let+ r =
      Lwt.catch
        (fun () ->
           loop Int64.(to_int (rem cur_slack (of_int handle.t.sector_size)))
             0 (String.length data))
        (fun exn ->
           free_blocks handle.t new_blocks;
           Lwt.reraise exn)
    in
    match r with
    | Error _ ->
      free_blocks handle.t new_blocks;
      r
    | Ok _ ->
      handle.blocks <- List.rev_append new_blocks handle.blocks;
      handle.length <- Int64.(add handle.length (of_int (String.length data)));
      r
end
