open Core_kernel.Std
open Bap.Std
open Bap_traces.Std

open Trace_tool_helpers

module Filter : sig

  type t = (frame -> bool)

  val enough_n : int -> t
  val each_nth : int -> t

end

(** [create ?is_good_enough trace] - return trace,
    with unique frames. [is_good_enough] could be used to
    constraing an occurrence of frames in trace  *)
val create : ?is_good_enough:(frame -> bool) -> trace -> trace
