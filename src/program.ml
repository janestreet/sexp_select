open! Base

module Action = struct
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

  let matches (atom : string) = function
    | `star -> true
    | `string s -> String.( = ) atom s
    | `one_of set -> Set.mem set atom
  ;;
end

type t = Action.t list [@@deriving sexp]
