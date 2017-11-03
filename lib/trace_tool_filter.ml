open Core_kernel.Std
open Bap.Std
open Bap_traces.Std

open Trace_tool_helpers

type t = (frame -> bool)

let get_code = Value.get Event.code_exec

let code_of_frame frame =
  Option.value_map ~default:None ~f:(fun c -> Some (Chunk.data c)) @@
  List.find_map ~f:get_code frame

let add numbers bytes =
  String.Table.change numbers bytes (function
      | None -> Some 1
      | Some n -> Some (n + 1))

let find_exn = String.Table.find_exn

let enough_n n =
  let numbers = String.Table.create () in
  fun frame ->
    match code_of_frame frame with
    | None -> false
    | Some bytes ->
      add numbers bytes;
      find_exn numbers bytes < n

let each_nth n =
  let numbers = String.Table.create () in
  fun frame ->
    match code_of_frame frame with
    | None -> false
    | Some bytes ->
      add numbers bytes;
      let x = find_exn numbers bytes mod n in
      x = 1 || x mod n = 0
