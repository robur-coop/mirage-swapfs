

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

module Make (_ : Mirage_block.S) : S
