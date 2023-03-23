open! Base


val select : string -> Sexp.t -> Sexp.t list

(** same as [select], but the parsing of the selection expression is done once so the
    parsed result can be shared across the evaluation of multiple sexps. *)
val select_staged : string -> (Sexp.t -> Sexp.t list) Staged.t
