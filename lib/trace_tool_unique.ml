open Core_kernel.Std
open Bap.Std
open Bap_traces.Std

open Trace_tool_helpers

let frame_to_string =
  List.fold ~init:"" ~f:(fun s e -> sprintf "%s%s;" s (Value.pps () e))

let unique_frames = Int.Table.create ()
let unique_bytes = String.Table.create ()

let is_unique_frame frame =
  let digest = Digest.string (frame_to_string frame) in
  let x = String.hash digest in
  Int.Table.add unique_frames x () = `Ok

module U(CPU : CPU) = struct

  let create_move_event tag cell' data' =
    Value.create tag Move.({cell = cell'; data = data';})

  let create_mem_store = create_move_event Event.memory_store
  let create_mem_load  = create_move_event Event.memory_load
  let create_reg_read  = create_move_event Event.register_read
  let create_reg_write = create_move_event Event.register_write
  let variables : var Int.Table.t = Int.Table.create ()

  let arbitary_var var =
    if CPU.is_flag var then var
    else
      match Var.typ var  with
      | Type.Mem _ -> var
      | Type.Imm x as typ ->
        match Int.Table.find variables x with
        | None ->
          let var = Var.create "tmp" typ in
          Int.Table.add_exn variables ~key:x ~data:var;
          var
        | Some var -> var

  let null word = Word.zero (Word.bitwidth word)

  let nullify_code_addr ch =
    let open Chunk in
    let chunk = {ch with addr = null (Chunk.addr ch);} in
    Value.create Event.code_exec chunk

  let nullify_pc_update addr =
    Value.create Event.pc_update (null addr)

  let nullify_addr ev =
    let open Move in
    Value.Match.(
      select @@
      case Event.memory_store (fun {cell; data} ->
          create_mem_store (null cell) data) @@
      case Event.memory_load (fun {cell; data} ->
          create_mem_load (null cell) data)  @@
      case Event.code_exec nullify_code_addr @@
      case Event.pc_update nullify_pc_update @@
      default (fun () -> ev)) ev

  let get_mem_addr e =
      Value.Match.(
        select @@
        case Event.memory_store (fun mv -> Some (Move.cell mv)) @@
        case Event.memory_load  (fun mv -> Some (Move.cell mv)) @@
        default (fun () -> None)) e

  let is_addr_data frame mv =
    let addrs = List.filter_map ~f:get_mem_addr frame in
    List.mem addrs (Move.data mv) ~equal:Word.equal

  let is_addr_in_reg frame mv =
    CPU.is_sp (Move.cell mv) || is_addr_data frame mv

  let is_flag mv = CPU.is_flag (Move.cell mv)

  let normalize frame =
    let open Move in
    let reg_read ({cell=var; data} as mv) =
      if is_addr_in_reg frame mv then None
      else Some (create_reg_read (arbitary_var var) data) in
    let reg_write ({cell=var; data} as mv) =
      if is_addr_in_reg frame mv then None
      else Some (create_reg_write (arbitary_var var) data) in
    let normalize_event ev =
      let ev = nullify_addr ev in
      Value.Match.(
        select @@
        case Event.register_read  reg_read  @@
        case Event.register_write reg_write @@
        default (fun () -> Some ev)) ev in
    List.filter_map ~f:normalize_event frame

  let is_unique frame =
    is_unique_frame (normalize frame)

  (** will filter all events with [f] + code exec  *)
  let filter_frame ~f frame =
    List.filter_map ~f:(fun e ->
        if Value.is Event.code_exec e then Some (nullify_addr e)
        else Option.some_if (f e) e) frame

  let checked_event tag f e =
    match Value.get tag e with
    | None -> false
    | Some x -> f x

  let filter_flags_writes frame =
    filter_frame ~f:(checked_event Event.register_write is_flag) frame

  let filter_flags_reads frame =
    filter_frame ~f:(checked_event Event.register_read is_flag) frame

  let is_unique_flags_writes frame =
    is_unique_frame (filter_flags_writes frame)

  let is_unique_flags_reads frame =
    is_unique_frame (filter_flags_reads frame)

  let is_unique_bytes frame =
    match List.find ~f:(Value.is Event.code_exec) frame with
    | None -> false
    | Some ev ->
      let ch = Value.get_exn Event.code_exec ev in
      String.Table.add unique_bytes (Chunk.data ch) () = `Ok

end

type t = [
  | `Normalized
  | `Flags_reads
  | `Flags_writes
  | `Bytes
]

let create u trace =
  let arch = Option.value_exn
    (Dict.find (Trace.meta trace) Meta.arch) in
  let module Target = (val (target_of_arch arch)) in
  let module U = U(Target.CPU) in
  let is_good_enough = match u with
    | `Normalized -> U.is_unique
    | `Flags_reads -> U.is_unique_flags_reads
    | `Flags_writes -> U.is_unique_flags_writes
    | `Bytes -> U.is_unique_bytes
  in
  let seq = make_frames ~is_good_enough trace in
  let fresh = Trace.create (Trace.tool trace) (make_unfold seq) in
  Trace.set_meta fresh (Trace.meta trace)
