open Core_kernel.Std
open Bap.Std
open Bap_traces.Std


(** Normalization:
    - no matter what the address is in memory load/store events
    - no matter what address is contained in any register
    - no matter what register is used for read/write events
    What does matter is what flags are used and what value are
    stored/loaded, wrote/read. And instruction (bytes) itself
    ofcourse.

    Uniqueness:
    Frame is unique if there weren't any frame before,
    (for given instruction) whoose normalized form is the same
    as current frame's form.

    Flags writes uniqueness is based on side effects of flags.
    Frame is unique in this case if there weren't any frame
    before (for given instruction), whoose flags writes were
    the same.

*)

type t = [
  | `Normalized
  | `Flags_reads
  | `Flags_writes
  | `Bytes
]

(** [create trace] - return trace with unique frames. *)
val create : t -> trace -> trace
