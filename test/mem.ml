type error = [
  | `Disconnected
  | `Not_sector_aligned
]

type write_error = [
  error
  | `Is_read_only
]

let pp_error ppf = function
  | #Mirage_block.error as e ->
    Mirage_block.pp_error ppf e
  | `Not_sector_aligned ->
    Format.pp_print_string ppf "buffers not sector aligned"

let pp_write_error ppf = function
  | #error as e -> pp_error ppf e
  | `Is_read_only as e ->
    Mirage_block.pp_write_error ppf e

type t = {
  info : Mirage_block.info;
  data : Cstruct.t;
}

let connect ?(read_write = true) ?(sector_size = 512) size_sectors =
  if sector_size <= 0 then
    invalid_arg "Mem.connect";
  if size_sectors < 0 then
    invalid_arg "Mem.connect";
  if size_sectors > max_int / sector_size then
    invalid_arg "Mem.connect";
  let info = { Mirage_block.read_write; sector_size; size_sectors = Int64.of_int size_sectors } in
  let data = Cstruct.create (size_sectors * sector_size) in
  { info; data }

let disconnect _t = Lwt.return_unit

let get_info t = Lwt.return t.info

let check_operation t _sector_start buffers =
  (* XXX: no bounds checks *)
  if List.exists
       (fun buf -> Cstruct.length buf mod t.info.sector_size <> 0)
       buffers then
    Error `Not_sector_aligned
  else Ok ()

let ( let* ) = Result.bind

let read t sector_start buffers =
  Lwt.return @@
  let* () = check_operation t sector_start buffers in
  let rec loop off = function
    | [] -> Ok ()
    | buf :: bufs ->
      let l = Cstruct.length buf in
      Cstruct.blit t.data off buf 0 l;
      loop (off + l) bufs
  in
  loop (Int64.to_int sector_start * t.info.sector_size) buffers

let write t sector_start buffers =
  Lwt.return @@
  let* () = check_operation t sector_start buffers in
  let rec loop off = function
    | [] -> Ok ()
    | buf :: bufs ->
      let l = Cstruct.length buf in
      Cstruct.blit buf 0 t.data off l;
      loop (off + l) bufs
  in
  loop (Int64.to_int sector_start * t.info.sector_size) buffers
