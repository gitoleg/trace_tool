open Core_kernel.Std
open Bap.Std
open Bap_plugins.Std
open Bap_traces.Std

open Trace_tool_helpers

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

let run_files files =
  let run_file file =
    let trace = trace_of_file file in
    Option.value_map trace ~default:None ~f:(fun trace ->
        let f1 = Unique.Filter.each_nth 10 in
        let f2 = Unique.Filter.enough_n 100 in
        let filter = Some (fun f -> f1 f && f2 f) in
        Some (Unique.create ?is_good_enough:filter trace)) in
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
  run None files

let run_unique path =
  let files =
    if Sys.is_directory path then (read_dir path)
    else [path] in
  match run_files files with
  | None -> eprintf "no trace produced: error during loading traces\n"
  | Some trace ->
    save_trace trace "unique"

let estimate_length file =
  match trace_of_file file with
  | None -> eprintf "something went wrong with %s\n" file;
  | Some trace ->
    let fs = make_frames trace in
    printf "file %s: %d\n" file (Seq.length fs)

type command =
  | Unique
  | Split
  | Append
  | Length

let command_of_string = function
  | "unique" -> Unique
  | "split"  -> Split
  | "append" -> Append
  | "length" -> Length
  | _ -> eprintf "unknown command\n"; exit 1

let run command path =
  match command_of_string command with
  | Unique -> run_unique path
  | Split -> Split.run path
  | Append -> eprintf "append is not implemented\n"; exit 1
  | Length -> estimate_length path

module Cmd = struct

  open Cmdliner

  let info =
    let doc = "Extract" in
    let man = [
      `S "DESCRIPTION";
      `P "A helpful tool to deal with unique frames";
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
