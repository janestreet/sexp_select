open! Core
open! Import

let example_sexp =
  Parsexp.Single.parse_string_exn
    {|
    ((foo bar)
     (foo not "a real field" (sausage apple))
     (baz (
       (sausage banana)
       (fred    george)
       (wizzle (
         (one   a)
         (two   b)
         (three c)))))
     (wizzle fizzle)
     (wizzle (
       (grizzle (
         (one z)
         (two y)))
       (drizzle chizzle)))
     (fred percy))
  |}
;;

let example_programs =
  [ "foo"
  ; "sausage"
  ; "fred"
  ; "baz fred"
  ; "two"
  ; "wizzle two"
  ; "wizzle > two"
  ; "wizzle > ( one two )"
  ; "wizzle ( one two )"
  ; "wizzle"
  ; "> wizzle"
  ; "wizzle > *"
  ]
;;

let completely_dropped = Sexp.Atom "<completely dropped>"

let%expect_test "select/deselect reference implementations" =
  let format_sexps sexps =
    List.map sexps ~f:Sexp.to_string_hum |> String.concat ~sep:"\n"
  in
  let print_if_different program f output1 output2 =
    if not (String.equal output1 output2)
    then (
      print_cr
        [%here]
        [%message "discrepancy" (program : string) (output1 : string) (output2 : string)];
      printf "%s [%s] vs [ref_%s]\n" program f f;
      print_patdiff ~context:0 ~location_style:None output1 output2;
      print_endline "")
  in
  let check_programs_on_sexp sexp programs =
    List.iter programs ~f:(fun program ->
      let selected = select program sexp |> format_sexps in
      let ref_selected = For_testing.ref_select program sexp |> format_sexps in
      print_if_different program "select" selected ref_selected;
      let sexp_option_to_string sexp_option =
        Option.value sexp_option ~default:completely_dropped |> Sexp.to_string_hum
      in
      let deselected = deselect program sexp |> sexp_option_to_string in
      let ref_deselected =
        For_testing.ref_deselect program sexp |> sexp_option_to_string
      in
      print_if_different program "deselect" deselected ref_deselected)
  in
  check_programs_on_sexp example_sexp example_programs;
  [%expect {| |}];
  (* These are examples where [select] has slightly unexpected behavior with how it
     matches a top-level key value pair. *)
  let weird_sexp = Parsexp.Single.parse_string_exn "(a (b (c (d 1))))" in
  let programs =
    [ "a"
    ; "> a"
    ; "b"
    ; "c"
    ; "d"
    ; "a b"
    ; "a > b"
    ; "a c"
    ; "a > c"
    ; "b d"
    ; "a b c d"
    ; "a > b > c > d"
    ]
  in
  check_programs_on_sexp weird_sexp programs;
  [%expect {| |}];
  (* An extra z at the end, so it's not a top level (key value) pair. *)
  let weird_sexp = Parsexp.Single.parse_string_exn "(a (b (c (d 1))) z)" in
  check_programs_on_sexp weird_sexp programs;
  [%expect {| |}]
;;
