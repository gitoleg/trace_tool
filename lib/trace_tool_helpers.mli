open Core_kernel.Std
open Bap.Std
open Bap_traces.Std

type event = Value.t [@@deriving bin_io, compare, sexp]
type frame = event list [@@deriving bin_io, compare, sexp]

val string_of_error : Trace.error -> string

val make_frames : ?is_good_enough:(frame -> bool) -> trace -> frame seq

val make_unfold : frame seq -> (unit -> event Or_error.t option)

val trace_of_file : string -> trace option

val save_trace : trace -> string -> unit

val append : trace -> trace -> trace
