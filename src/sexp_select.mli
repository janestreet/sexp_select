open! Base

val select : string -> Sexp.t -> Sexp.t list

(** same as [select], but the parsing of the selection expression is done once so the
    parsed result can be shared across the evaluation of multiple sexps. *)
val select_staged : string -> (Sexp.t -> Sexp.t list) Staged.t

(** Like select, but returns a single element. Raises if that is not possible (e.g. if
    there are no matches or if there are multiple matches) *)
val select_single_exn : string -> Sexp.t -> Sexp.t

(** run a list of programs on an input sexp and nicely format the programs and their
    corresponding outputs when run on the given sexp, for use in -help output and
    expect tests. *)
val format_program_outputs : Sexp.t -> string list -> string

(** invert the behavior of [select]; drop the fields matching the selection expression. *)
val deselect : string -> Sexp.t -> Sexp.t option

(** same as [deselect], but the parsing of the selection expression is done once so the
    parsed result can be shared across the evaluation of multiple sexps. *)
val deselect_staged : string -> (Sexp.t -> Sexp.t option) Staged.t

(** Reference implementations of [select] and [deselect] that are linked, to verify that
    operations are inversions of each other.

    We don't use these as the actual implementations because they allocate more more than
    the standalone versions (especially for [select]) and we don't want to always pay for
    that unnecessary overhead.
*)
module For_testing : sig
  module Program = Program
  module Parse = Parse

  val ref_select : string -> Sexp.t -> Sexp.t list
  val ref_deselect : string -> Sexp.t -> Sexp.t option
end
