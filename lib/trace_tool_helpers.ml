open Core_kernel.Std
open Bap.Std
open Bap_traces.Std

type event = Value.t [@@deriving bin_io, compare, sexp]
type frame = event list [@@deriving bin_io, compare, sexp]

let string_of_error = function
  | `Protocol_error er ->
    Printf.sprintf "protocol error: %s"
      (Info.to_string_hum (Error.to_info er))
  | `System_error er ->
    Printf.sprintf "system error: %s" (Unix.error_message er)
  | `No_provider -> "no provider"
  | `Ambiguous_uri -> "ambiguous uri"

let name_of_trace trace =
  match Dict.find (Trace.meta trace) Meta.binary with
  | None -> None
  | Some bin ->
    let p = bin.Binary.path in
    Some (Filename.basename p)

let all_good _ = true

let is_pc_update = Value.is Event.pc_update
let is_code_exec = Value.is Event.code_exec

let make_frames ?(is_good_enough=all_good) trace =
  let frame_is_ready frame =
    let has_pc_update = List.exists ~f:is_pc_update frame in
    let has_code_exec = List.exists ~f:is_code_exec frame in
    has_pc_update && has_code_exec in
  let step frame ev =
    if is_pc_update ev && frame_is_ready frame then
      if is_good_enough frame then
        Seq.Step.Yield (List.rev frame, [ev])
      else
        Seq.Step.Skip ([ev])
    else
      Seq.Step.Skip (ev :: frame) in
  Seq.unfold_with (Trace.read_events trace) ~init:[] ~f:step

let make_unfold fs =
  let frame = ref [] in
  let frames = ref fs in
  let rec unfold_internal () = match !frame with
    | ev :: evs ->
      frame := evs;
      Some (Ok  ev)
    | [] -> match Seq.next !frames with
      | None -> None
      | Some (new_frame, rest) ->
        frame := new_frame;
        frames := rest;
        unfold_internal () in
  unfold_internal

let trace_of_file file =
  let uri = Uri.of_string ("file:" ^ file) in
  match Trace.load uri with
  | Error er ->
    eprintf "error during loading trace: %s\n" (string_of_error er);
    None
  | Ok trace -> Some trace

let save_trace trace file =
  let file = sprintf "file:%s.binprot" file in
  let uri = Uri.of_string file in
  match Trace.save uri trace with
  | Ok () -> ()
  | Error er -> eprintf "error %s \n" (string_of_error er)

let append trace trace' =
  let events = Trace.read_events trace in
  let events' = Trace.read_events trace' in
  let events = Seq.append events events' in
  let unfold () =
    let evs = ref events in
    fun () -> match Seq.next !evs with
      | None -> None
      | Some (ev, rest) ->
        evs := rest;
        Some (Ok ev) in
  let fresh = Trace.create (Trace.tool trace) (unfold ()) in
  Trace.set_meta fresh (Trace.meta trace)
