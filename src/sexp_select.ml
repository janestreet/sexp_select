open! Base

module Eval = struct
  let rec descendants name : Sexp.t -> _ = function
    | List [ Atom key; value ] when Program.Action.matches key name ->
      value :: descendants name value
    | List l -> List.bind l ~f:(descendants name)
    | Atom _ -> []
  ;;

  let children name : Sexp.t -> _ = function
    | Atom _ -> []
    | List l ->
      List.filter_map l ~f:(function
        | List [ Atom key; value ] ->
          if Program.Action.matches key name then Some value else None
        | List ([] | [ _ ] | [ List _; _ ] | _ :: _ :: _ :: _) | Atom _ -> None)
  ;;
end

let rec eval ~(program : Program.t) sexp =
  match program with
  | [] -> [ sexp ]
  | `descendants ident :: rest ->
    List.bind (Eval.descendants ident sexp) ~f:(eval ~program:rest)
  | `children ident :: rest ->
    List.bind (Eval.children ident sexp) ~f:(eval ~program:rest)
;;

let select_staged program_string =
  let program = Parse.parse program_string in
  Staged.stage (fun sexp -> eval ~program sexp)
;;

let select program_string sexp =
  let select_fn = Staged.unstage (select_staged program_string) in
  select_fn sexp
;;

let select_single_exn label sexp =
  match select label sexp with
  | [ single ] -> single
  | [] -> raise_s [%message "Found no matches."]
  | _ :: _ :: _ -> raise_s [%message "Found multiple matches."]
;;

module Filter = struct
  let rec filter (sexp : Sexp.t) (actions : Program.Action.t Nonempty_list.t) =
    match sexp with
    | Atom _ -> `unchanged
    | List l ->
      let (head_action :: remaining_actions) = actions in
      let remaining_actions = Nonempty_list.of_list remaining_actions in
      let match_at_lower_levels_if_no_match, curr_action =
        match head_action with
        | `descendants action -> true, action
        | `children action -> false, action
      in
      (match l with
       (* This branch is to handle the weird behavior of select where the program
          "a" will match the top-level "(a 1)" and return 1, but "> a" won't.
          In that case, [filter] won't return anything; the entire sexp match the
          select program, so we'll return [None]. *)
       | [ (Atom k as key); value ]
         when Program.Action.matches k curr_action && match_at_lower_levels_if_no_match ->
         (match remaining_actions with
          | None -> `changed None
          | Some remaining_actions ->
            (match filter value remaining_actions with
             | `unchanged -> `unchanged
             | `changed None -> `changed (Some (Sexp.List [ key ]))
             | `changed (Some new_value) -> `changed (Some (List [ key; new_value ]))))
       | _ ->
         let did_change, l =
           List.fold ~init:(false, []) l ~f:(fun (did_change, l) elem ->
             match elem with
             | List [ (Atom k as key); value ] when Program.Action.matches k curr_action
               ->
               (match remaining_actions with
                | None -> true, l
                | Some remaining_actions ->
                  (match filter value remaining_actions with
                   | `unchanged -> did_change, elem :: l
                   | `changed None -> true, List [ key ] :: l
                   | `changed (Some new_value) -> true, List [ key; new_value ] :: l))
             | _ ->
               if match_at_lower_levels_if_no_match
               then (
                 match filter elem actions with
                 | `unchanged -> did_change, elem :: l
                 | `changed None -> true, l
                 | `changed (Some new_elem) -> true, new_elem :: l)
               else did_change, elem :: l)
         in
         if did_change then `changed (Some (List (List.rev l))) else `unchanged)
  ;;

  let apply sexp actions =
    match filter sexp actions with
    | `unchanged -> Some sexp
    | `changed new_sexp -> new_sexp
  ;;
end

(* Produce a table that looks like this:

   {v
     program1        -> [ (first output); second_output ]
     another program -> [ output ]
     third > program -> [ (some very long output);
                          and_another_output ]
   v}

   where the left side is the programs passed in, and the right side is the outputs
   of running [select] on the input sexp.
*)
let format_program_outputs sexp programs =
  let max_program_width =
    List.fold programs ~init:0 ~f:(fun prev_max program ->
      Int.max (String.length program) prev_max)
  in
  let pad s = s ^ String.make (max_program_width - String.length s) ' ' in
  let output_padding = String.make (max_program_width + String.length " -> [ ") ' ' in
  List.map programs ~f:(fun program ->
    let output = select program sexp |> List.map ~f:Sexp.to_string_mach in
    let output_str =
      let one_line = String.concat ~sep:"; " output in
      if String.length one_line <= 40
      then one_line
      else
        (match output with
         | [] -> assert false
         | head :: tail -> head :: List.map tail ~f:(fun s -> output_padding ^ s))
        |> String.concat ~sep:";\n"
    in
    Printf.sprintf "%s -> [ %s ]" (pad program) output_str)
  |> String.concat ~sep:"\n"
;;

let deselect_staged program_string =
  let actions = Parse.parse program_string in
  match Nonempty_list.of_list actions with
  | None -> Printf.failwithf "must specify at least one selector" ()
  | Some actions -> Staged.stage (fun sexp -> Filter.apply sexp actions)
;;

let deselect program_string sexp =
  let deselect_fn = Staged.unstage (deselect_staged program_string) in
  deselect_fn sexp
;;

module For_testing = struct
  module Program = Program
  module Parse = Parse

  (* the reference implementations of select and deselect are written in a combined way so
     that their semantics are more likely to be consistent with each other. *)
  let rec split (sexp : Sexp.t) (actions : Program.Action.t Nonempty_list.t) =
    match sexp with
    | Atom _ -> [], Some sexp
    | List l ->
      let (head_action :: remaining_actions) = actions in
      let remaining_actions = Nonempty_list.of_list remaining_actions in
      let match_at_lower_levels_if_no_match, curr_action =
        match head_action with
        | `descendants action -> true, action
        | `children action -> false, action
      in
      (match l with
       (* This branch is to handle the weird behavior of select where the program
          "a" will match the top-level "(a 1)" and return 1, but "> a" won't.
          In that case, if this is the end of the program, we'll select the value
          and nothing will remain. If there's more to the program then we'll continue
          selecting the value. *)
       | [ (Atom k as key); value ]
         when Program.Action.matches k curr_action && match_at_lower_levels_if_no_match ->
         (match remaining_actions with
          | None -> [ value ], None
          | Some remaining_actions ->
            let selected, new_value = split value remaining_actions in
            (match new_value with
             | None -> selected, Some (Sexp.List [ key ])
             | Some new_value -> selected, Some (List [ key; new_value ])))
       | _ ->
         let selected, remaining =
           List.fold
             ~init:([], Reversed_list.[])
             l
             ~f:(fun (selected, remaining) elem ->
               match elem with
               | List [ (Atom k as key); value ] when Program.Action.matches k curr_action
                 ->
                 (match remaining_actions with
                  | None -> value :: selected, remaining
                  | Some remaining_actions ->
                    let nested_selected, new_value = split value remaining_actions in
                    let new_remaining : Sexp.t Reversed_list.t =
                      match new_value with
                      | None -> List [ key ] :: remaining
                      | Some new_value -> List [ key; new_value ] :: remaining
                    in
                    List.rev_append nested_selected selected, new_remaining)
               | _ ->
                 if match_at_lower_levels_if_no_match
                 then (
                   let nested_selected, new_elem = split elem actions in
                   let selected = List.rev_append nested_selected selected in
                   let remaining : Sexp.t Reversed_list.t =
                     match new_elem with
                     | None -> remaining
                     | Some new_elem -> new_elem :: remaining
                   in
                   selected, remaining)
                 else selected, elem :: remaining)
         in
         List.rev selected, Some (Sexp.List (Reversed_list.rev remaining)))
  ;;

  let ref_select program_string sexp =
    let actions = Parse.parse program_string in
    match Nonempty_list.of_list actions with
    | None -> []
    | Some actions -> split sexp actions |> fst
  ;;

  let ref_deselect program_string sexp =
    let actions = Parse.parse program_string in
    match Nonempty_list.of_list actions with
    | None -> Printf.failwithf "must specify at least one selector" ()
    | Some actions -> split sexp actions |> snd
  ;;
end
