open Core_kernel.Std
open Bap.Std
open Bap_traces.Std

open Trace_tool_helpers

(** Some filters, that work on instruction level, where instruction
    is a unique bytes sequence *)

type t = (frame -> bool)

(** [enough_n n] return true if less then [n] instructions were
    occured during processing trace(s) *)
val enough_n : int -> t

(** [enough_n n] return true for every [n]th instruction *)
val each_nth : int -> t
