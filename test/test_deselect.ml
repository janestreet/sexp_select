open! Core
open! Import

let test_sexp_string =
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

let to_string_diffable sexp =
  let rec extract_values sexp ~path =
    match sexp with
    | Sexp.Atom _ -> [ path, Sexp.to_string sexp ]
    | Sexp.List l ->
      List.map l ~f:(fun value ->
        match value with
        | Sexp.List [ Sexp.Atom key; value ] ->
          let path = path ^ "." ^ key in
          extract_values value ~path
        | _ ->
          let path = path ^ ".[]" in
          extract_values value ~path)
      |> List.concat
  in
  let paths_and_values = extract_values sexp ~path:"" in
  let max_path_len =
    List.map paths_and_values ~f:fst
    |> List.map ~f:String.length
    |> List.max_elt ~compare:Int.compare
    |> Option.value_exn
  in
  List.map paths_and_values ~f:(fun (path, value) ->
    let padding = String.make (max_path_len - String.length path) ' ' in
    path ^ padding ^ " " ^ value)
  |> String.concat ~sep:"\n"
;;

let test_sexp () = Parsexp.Single.parse_string_exn test_sexp_string

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

let%expect_test "deselect" =
  let test_sexp = test_sexp () in
  Core.print_endline (to_string_diffable test_sexp);
  [%expect
    {|
    .foo                bar
    .[].[]              foo
    .[].[]              not
    .[].[]              "a real field"
    .[].sausage         apple
    .baz.sausage        banana
    .baz.fred           george
    .baz.wizzle.one     a
    .baz.wizzle.two     b
    .baz.wizzle.three   c
    .wizzle             fizzle
    .wizzle.grizzle.one z
    .wizzle.grizzle.two y
    .wizzle.drizzle     chizzle
    .fred               percy
    |}];
  Core.print_endline (to_string_diffable test_sexp);
  let original_sexp = [%expect.output] in
  List.iter example_programs ~f:(fun program ->
    let output = deselect program test_sexp |> Option.value ~default:(Atom "<dropped>") in
    Core.printf "%s" ("Deselect: " ^ program);
    print_patdiff
      ~context:0
      ~location_style:None
      original_sexp
      (to_string_diffable output);
    Core.print_endline "");
  [%expect
    {|
    Deselect: foo
    -|.foo                bar

    Deselect: sausage
    -|.[].sausage         apple
    -|.baz.sausage        banana

    Deselect: fred
    -|.baz.fred           george

    -|.fred               percy

    Deselect: baz fred
    -|.baz.fred           george

    Deselect: two
    -|.baz.wizzle.two     b

    -|.wizzle.grizzle.two y

    Deselect: wizzle two
    -|.baz.wizzle.two     b

    -|.wizzle.grizzle.two y

    Deselect: wizzle > two
    -|.baz.wizzle.two     b

    Deselect: wizzle > ( one two )
    -|.baz.wizzle.one     a
    -|.baz.wizzle.two     b

    Deselect: wizzle ( one two )
    -|.baz.wizzle.one     a
    -|.baz.wizzle.two     b

    -|.wizzle.grizzle.one z
    -|.wizzle.grizzle.two y

    Deselect: wizzle
    -|.baz.wizzle.one     a
    -|.baz.wizzle.two     b
    -|.baz.wizzle.three   c
    -|.wizzle             fizzle
    -|.wizzle.grizzle.one z
    -|.wizzle.grizzle.two y
    -|.wizzle.drizzle     chizzle

    Deselect: > wizzle
    -|.wizzle             fizzle
    -|.wizzle.grizzle.one z
    -|.wizzle.grizzle.two y
    -|.wizzle.drizzle     chizzle

    Deselect: wizzle > *
    -|.baz.wizzle.one     a
    -|.baz.wizzle.two     b
    -|.baz.wizzle.three   c
    -|.wizzle             fizzle
    -|.wizzle.grizzle.one z
    -|.wizzle.grizzle.two y
    -|.wizzle.drizzle     chizzle
    +|.wizzle      fizzle
    |}]
;;

let%expect_test "unexpected behavior" =
  (* [select] does slightly unexpected things here, so let's just document what [delesect]
     does as well. *)
  let sexp = Parsexp.Single.parse_string_exn "(a (b (c (d 1))))" in
  let programs =
    [ "a"
    ; "> a"
    ; "b"
    ; "c"
    ; "d"
    ; "a b"
    ; "a > b"
    ; "a c"
    ; "b d"
    ; "a b c d"
    ; "a > b > c > d"
    ]
  in
  List.iter programs ~f:(fun program ->
    print_endline ("deselect \"" ^ program ^ "\" ->");
    print_s (deselect program sexp |> Option.value ~default:(Atom "<completely dropped>"));
    print_endline "");
  [%expect
    {|
    deselect "a" ->
    "<completely dropped>"

    deselect "> a" ->
    (a (b (c (d 1))))

    deselect "b" ->
    (a)

    deselect "c" ->
    (a (b))

    deselect "d" ->
    (a (b (c)))

    deselect "a b" ->
    (a)

    deselect "a > b" ->
    (a (b (c (d 1))))

    deselect "a c" ->
    (a (b))

    deselect "b d" ->
    (a (b (c)))

    deselect "a b c d" ->
    (a (b (c)))

    deselect "a > b > c > d" ->
    (a (b (c (d 1))))
    |}]
;;
