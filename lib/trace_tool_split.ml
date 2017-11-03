open Core_kernel.Std
open Bap.Std
open Bap_traces.Std

open Trace_tool_helpers

module Dis = Disasm_expert.Basic

let get_code = Value.get Event.code_exec

let create_dis arch =
  Dis.create ~backend:"llvm" (Arch.to_string arch) |>
  Or_error.ok_exn |>
  Dis.store_kinds |>
  Dis.store_asm

let name_of_chunk dis endian chunk =
  let mem =
    Bigstring.of_string (Chunk.data chunk) |>
    Memory.create endian (Chunk.addr chunk) in
  let mem = Or_error.ok_exn mem in
  match Dis.insn_of_mem dis mem with
  | Ok (_, Some insn, _) ->
    let name = Dis.Insn.name insn in
    Some name
  | _ -> None

let insn_name dis endian frame =
  match List.find_map ~f:get_code frame with
  | None -> None
  | Some chunk ->
    name_of_chunk dis endian chunk

let names trace =
  let names = String.Set.empty in
  let arch = Option.value_exn (Dict.find (Trace.meta trace) Meta.arch) in
  let endian = Arch.endian arch in
  let dis = create_dis arch in
  Seq.fold (Trace.read_events trace)
    ~init:names
    ~f:(fun names ev ->
        match Value.get Event.code_exec ev with
        | None -> names
        | Some chunk ->
          let name = name_of_chunk dis endian chunk in
          Option.fold name ~init:names ~f:(fun names n ->
              String.Set.add names n))

let filter_by_name ?f trace name =
  let arch = Option.value_exn (Dict.find (Trace.meta trace) Meta.arch) in
  let endian = Arch.endian arch in
  let dis = create_dis arch in
  let filter_name = match f with
    | Some f -> f
    | None -> fun insn_name -> String.equal name insn_name in
  let filter f =
    insn_name dis endian f |>
    Option.value_map ~default:false ~f:filter_name in
  let fs = make_frames ~is_good_enough:filter trace in
  let fresh = Trace.create (Trace.tool trace) (make_unfold fs) in
  Trace.set_meta fresh (Trace.meta trace)
