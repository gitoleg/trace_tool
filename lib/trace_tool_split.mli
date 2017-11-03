open Core_kernel.Std
open Bap.Std
open Bap_traces.Std

(** [names trace] - returns a list of unique instructions names in the
    [trace]. *)
val names : trace -> String.Set.t

(** [filter_by_name ?f trace name] - returns a trace with only [name] instructions *)
val filter_by_name : ?f:(string -> bool) -> trace -> string -> trace
