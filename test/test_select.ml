open! Core
open! Import

let%expect_test "select" =
  let sexp =
    Parsexp.Single.parse_string_exn
      {|
    ((foo bar)
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
  in
  let programs =
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
  in
  print_endline (format_program_outputs sexp programs);
  [%expect
    {|
    foo                  -> [ bar ]
    sausage              -> [ banana ]
    fred                 -> [ george; percy ]
    baz fred             -> [ george ]
    two                  -> [ b; y ]
    wizzle two           -> [ b; y ]
    wizzle > two         -> [ b ]
    wizzle > ( one two ) -> [ a; b ]
    wizzle ( one two )   -> [ a; b; z; y ]
    wizzle               -> [ ((one a)(two b)(three c));
                              fizzle;
                              ((grizzle((one z)(two y)))(drizzle chizzle)) ]
    > wizzle             -> [ fizzle;
                              ((grizzle((one z)(two y)))(drizzle chizzle)) ]
    wizzle > *           -> [ a; b; c; ((one z)(two y)); chizzle ] |}]
;;

let%expect_test "select_single_exn" =
  let sexp =
    Parsexp.Single.parse_string_exn
      {|
    ((foo bar)
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
  in
  let bad_programs = [ "one"; "cowabunga" ] in
  let good_programs = [ "foo" ] in
  let test program =
    print_s [%message (program : string)];
    let sexp = Sexp_select.select_single_exn program sexp in
    print_s [%message (sexp : Sexp.t)]
  in
  List.iter good_programs ~f:test;
  [%expect {|
    (program foo)
    (sexp bar) |}];
  List.iter bad_programs ~f:(fun program ->
    Expect_test_helpers_base.require_does_raise [%here] (fun () -> test program));
  [%expect
    {|
    (program one)
    "Found multiple matches."
    (program cowabunga)
    "Found no matches." |}]
;;

let%expect_test "weird edge cases" =
  let sexp1 = Parsexp.Single.parse_string_exn "(a (b (c (d 1))))" in
  (* An extra z at the end, so it's not a top level (key value) pair. *)
  let sexp2 = Parsexp.Single.parse_string_exn "(a (b (c (d 1))) z)" in
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
  print_endline (format_program_outputs sexp1 programs);
  [%expect
    {|
    a             -> [ (b(c(d 1))) ]
    > a           -> [  ]
    b             -> [ (c(d 1)) ]
    c             -> [ (d 1) ]
    d             -> [ 1 ]
    a b           -> [ (c(d 1)) ]
    a > b         -> [  ]
    a c           -> [ (d 1) ]
    a > c         -> [ (d 1) ]
    b d           -> [ 1 ]
    a b c d       -> [ 1 ]
    a > b > c > d -> [  ] |}];
  (* Programs including an "a" don't select anything anymore. *)
  print_endline (format_program_outputs sexp2 programs);
  [%expect
    {|
    a             -> [  ]
    > a           -> [  ]
    b             -> [ (c(d 1)) ]
    c             -> [ (d 1) ]
    d             -> [ 1 ]
    a b           -> [  ]
    a > b         -> [  ]
    a c           -> [  ]
    a > c         -> [  ]
    b d           -> [ 1 ]
    a b c d       -> [  ]
    a > b > c > d -> [  ] |}]
;;
