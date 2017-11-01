open Core_kernel.Std
open Bap.Std
open Bap_traces.Std

open Trace_tool_helpers

module Filter = struct

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
end

module U = struct

  let uniques = Int.Table.create ()

  let to_string =
    List.fold ~init:"" ~f:(fun s e -> sprintf "%s%s;" s (Value.pps () e))

  let is_unique frame =
    let x = String.hash (to_string frame) in
    Int.Table.add uniques x () = `Ok
end

let create ?is_good_enough trace =
  let check_unique frame = U.is_unique frame in
  let is_good_enough f = match is_good_enough with
    | None -> check_unique f
    | Some is_good -> check_unique f && is_good f in
  let seq = make_frames ~is_good_enough trace in
  let fresh = Trace.create (Trace.tool trace) (make_unfold seq) in
  Trace.set_meta fresh (Trace.meta trace)
