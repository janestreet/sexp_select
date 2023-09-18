open! Base

module Action : sig
  type ident =
    [ `star
    | `string of string
    | `one_of of Set.M(String).t
    ]
  [@@deriving sexp]

  type t =
    [ `descendants of ident (* All descendants matching [ident] *)
    | `children of ident (* All direct children matching [ident] *)
    ]
  [@@deriving sexp]

  val matches : string -> ident -> bool
end

type t = Action.t list [@@deriving sexp]
