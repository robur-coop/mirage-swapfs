

module type S = sig
  type t
  (** The swap filesystem *)

  type handle
  (** A handle for a growable unnamed file on the swap filesystem *)

  type error

  val pp_error : Format.formatter -> error -> unit

  val empty : t -> handle
  (** [empty t] is a handle [handle] for an empty unnamed file.
      [Gc.finalise free handle] is called so garbage collected handles
      automatically result in freed blocks. However, it is recommended to call
      [free handle] manually earlier. *)

  val append : handle -> string -> (unit, error) result Lwt.t
  (** Write data to the end of the unnamed file. Block allocations are done as needed. *)

  val size : handle -> int64
  (** [size handle] is the length of the unnamed file represented by [handle] *)

  val get_partial : handle -> offset:int64 -> length:int -> (string, error) result Lwt.t
  (** [get_partial handle ~offset ~length] reads [length] bytes at offset
      [offset] from the unnamed file represented by [handle]. Errors can
      happen. *)

  val free : handle -> unit Lwt.t
  (** [free handle] marks all blocks allocated for [handle] as free and resets
      [handle] to zero length. [handle] can be reused. *)
end

module Make (B : Mirage_block.S) : sig
  include S
  val connect : ?blocking_factor: int -> B.t -> t Lwt.t
  (** [connect ~blocking_factor block] is a swap filesystem backed by [block].
      [blocking_factor] is the minimum number of sectors to allocate. The
      higher the value the less memory is used by the swap filesystem at the
      cost of less efficient use of the block device. The default value is 2048
      which is 1 MiB with a sector size of 512. *)
end
