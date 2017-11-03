open Core_kernel.Std
open Bap.Std
open Bap_plugins.Std
open Bap_traces.Std

open Trace_tool_helpers

module Filter = Trace_tool_filter
module Split  = Trace_tool_split
module Unique = Trace_tool_unique

let () =
  match Plugins.load () |> Result.all with
  | Ok plugins -> ()
  | Error (path, er) ->
    Printf.eprintf "failed to load plugin from %s: %s"
      path (Error.to_string_hum er)

let read_dir path =
  let dir = Unix.opendir path in
  let fullpath file = String.concat ~sep:"/" [path; file] in
  let is_trace file =
    Filename.check_suffix file ".frames" ||
    Filename.check_suffix file ".binprot" in
  let next () =
    try
      Some (Unix.readdir dir)
    with End_of_file -> None in
  let rec folddir acc =
    match next () with
    | Some file ->
      if is_trace file then
        folddir (Set.add acc (fullpath file))
      else folddir acc
    | None -> acc in
  let files = folddir String.Set.empty in
  Unix.closedir dir;
  Set.to_list files

let run_unique u path =
  let run_file file =
    let trace = trace_of_file file in
    Option.value_map trace ~default:None ~f:(fun trace ->
        Some (Unique.create u trace)) in
  let rec run trace = function
    | [] -> trace
    | f :: fs ->
      match run_file f with
      | None -> run trace fs
      | Some unique_trace ->
        match trace with
        | None -> run (Some unique_trace) fs
        | Some trace ->
          let trace = append trace unique_trace in
          run (Some trace) fs in
  let files =
    if Sys.is_directory path then (read_dir path)
    else [path] in
  match run None files with
  | None ->
    eprintf "no trace produced: error during processing traces\n"
  | Some trace -> save_trace trace "unique"

let ensure_file path =
  if Sys.is_directory path then
    let () = eprintf "%s should be regular file\n" path in
    exit 1

let run_split path =
  ensure_file path;
  match trace_of_file path with
  | None -> eprintf "failed to load %s\n" path; exit 1
  | Some trace ->
    let insn_group name = String.filter ~f:Char.is_uppercase name in
    let names =
      Set.fold ~init:String.Set.empty (Split.names trace)
        ~f:(fun names name ->
            String.Set.add names (insn_group name)) in
    let save_i name =
      match trace_of_file path with
      | None -> eprintf "something went wrong with %s\n" path;
      | Some trace ->
        let trace = Split.filter_by_name
            ~f:(fun i -> insn_group i = name) trace name in
        save_trace trace name in
    Set.iter ~f:(fun n -> save_i n) names

let length ?(full=false) file =
  ensure_file file;
  match trace_of_file file with
  | None -> eprintf "something went wrong with %s\n" file;
  | Some trace ->
    if full then
      printf "%d\n" (Seq.length (Trace.read_events trace))
    else
      let fs = make_frames trace in
      printf "%d\n" (Seq.length fs)

type command =
  | Unique of Unique.t
  | Split
  | Length
  | Size

let command_of_string = function
  | "unique" -> Unique `Normalized
  | "r-flags" -> Unique `Flags_reads
  | "w-flags" -> Unique `Flags_writes
  | "bytes" -> Unique `Bytes
  | "split"  -> Split
  | "length" -> Length
  | "size"   -> Size
  | _ -> eprintf "unknown command\n"; exit 1

let run command path =
  match command_of_string command with
  | Unique x -> run_unique x path
  | Split  -> run_split path
  | Length -> length path
  | Size -> length ~full:true path

module Cmd = struct

  open Cmdliner

  let info =
    let doc = "Trace-tool" in
    let man = [
      `S "DESCRIPTION";
      `P "A helpful tool to deal with unique frames";
      `Pre "The next commands available:
unique  - produce a trace with unique frames
r-flags - produce a trace with unique frames, where uniqueness is based on flags reads
w-flags - produce a trace with unique frames, where uniqueness is based on flags writes
bytes   - produce a trace with unique frames, where uniqueness is based on insn opcode
split   - just split a trace into a smaller traces, with same instruction group each
length  - returns a number of instructions in trace
size    - return a number of events in trace"
    ] in
    Term.info "extract" ~doc ~man

  let command =
    let doc = "Command for processing trace file(s)" in
    Arg.(required & pos 0 (some string) None & info [] ~doc ~docv:"Command")

  let path =
    let doc = "Input trace file or directory with trace files" in
    Arg.(required & pos 1 (some string) None & info [] ~doc ~docv:"FILE | DIR")

  let run_t = Term.(const run $ command $ path)

  let () =
    match Term.eval (run_t, info) ~catch:false with
    | `Ok () -> ()
    | `Error `Parse -> exit 64
    | `Error _ -> exit 2
    | _ -> exit 1

end
