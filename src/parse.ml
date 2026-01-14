open! Base

let parse_ident tokens =
  match tokens with
  | Sexp.Atom "*" -> `star
  | Sexp.Atom ident -> `string ident
  | Sexp.List idents ->
    (* Nested parentheses are meaningless. *)
    let get_only_strings = function
      | Sexp.Atom s -> s
      | Sexp.List _ as tokens ->
        Printf.failwithf
          "nested parens are not supported: '%s'"
          (Sexp.to_string tokens)
          ()
    in
    `one_of (List.map idents ~f:get_only_strings |> Set.of_list (module String))
;;

let parse_one = function
  (* This actually needn't return option, since we always have a valid parse. But we leave
     it as-is for future language extensions that might be more restrictive. *)
  | [] -> None
  | Sexp.Atom ">" :: unparsed_ident :: rest ->
    let ident = parse_ident unparsed_ident in
    Some (`children ident, rest)
  | ident :: rest -> Some (`descendants (parse_ident ident), rest)
;;

let parse s =
  let rec loop tokens =
    match parse_one tokens with
    (* Needed, because we might use multiple symbols at one (> asdf -> `children asdf, []) *)
    | Some (action, rest) -> action :: loop rest
    | None -> []
  in
  (* We parse the query by leveraging regular Sexp parsing to ensure parentheses are
     matched correctly and to handle the grouping automatically. *)
  loop (Parsexp.Many.parse_string_exn s)
;;
