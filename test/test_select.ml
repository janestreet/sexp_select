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
