open! Core
open Import
module Parse = For_testing.Parse
module Program = For_testing.Program

let print_parsed program =
  let program = Parse.parse program in
  print_s [%sexp (program : Program.t)]
;;

let%expect_test {|A pair of quotes (old behaviour of "\"\"")|} =
  (* Unescaped: "\"\"" *)
  print_parsed "\"\\\"\\\"\"";
  [%expect {| ((descendants (string "\"\""))) |}]
;;

let%expect_test "atoms can contain spaces" =
  print_parsed "\"Hello World\"";
  [%expect {|
    ((descendants (string "Hello World"))) |}]
;;

let%expect_test "Single empty string" =
  print_parsed "\"\"";
  [%expect {| ((descendants (string ""))) |}]
;;

let%expect_test "Single space" =
  print_parsed "\" \"";
  [%expect {| ((descendants (string " "))) |}]
;;

let%expect_test "foo" =
  print_parsed "foo";
  [%expect {| ((descendants (string foo))) |}]
;;

let%expect_test "descendants" =
  print_parsed "baz fred";
  [%expect {|
    ((descendants (string baz))
     (descendants (string fred))) |}]
;;

let%expect_test "direct children" =
  print_parsed "wizzle > two";
  [%expect {|
    ((descendants (string wizzle))
     (children    (string two))) |}]
;;

let%expect_test "star operator" =
  print_parsed "wizzle > *";
  [%expect {| ((descendants (string wizzle)) (children star)) |}]
;;

let%expect_test "match one of many" =
  print_parsed "( a b )";
  let output = [%expect.output] in
  print_endline output;
  [%expect {|
    ((descendants (one_of (a b)))) |}];
  print_parsed "(a b)";
  Expect_test_patdiff.print_patdiff output [%expect.output];
  [%expect {||}]
;;

let%expect_test "match one of many direct children" =
  print_parsed "wizzle > ( one two )";
  [%expect {| ((descendants (string wizzle)) (children (one_of (one two)))) |}]
;;

let%expect_test "complex program" =
  print_parsed "a ( b c d ) > ( e f g ) h i ( a 1 )";
  let output = [%expect.output] in
  print_endline output;
  [%expect
    {|
    ((descendants (string a))
     (descendants (one_of (b c d)))
     (children    (one_of (e f g)))
     (descendants (string h))
     (descendants (string i))
     (descendants (one_of (1 a)))) |}];
  print_parsed "a(b c d)>(e f g)h i(a 1)";
  Expect_test_patdiff.print_patdiff output [%expect.output];
  [%expect {||}]
;;

let%expect_test "match one of many direct children" =
  print_parsed "wizzle > ( one two )";
  [%expect {| ((descendants (string wizzle)) (children (one_of (one two)))) |}]
;;

(* Edge cases *)
let%expect_test "Leading >" =
  print_parsed "> a";
  [%expect {| ((children (string a))) |}]
;;

let%expect_test "Trailing >" =
  (* Should maybe be a parse error? *)
  print_parsed "foo >";
  [%expect {|
    ((descendants (string foo))
     (descendants (string >))) |}]
;;

let%expect_test "Adjacent >" =
  (* Should maybe be a parse error? *)
  print_parsed "foo > > bar";
  [%expect
    {|
    ((descendants (string foo))
     (children    (string >))
     (descendants (string bar))) |}]
;;

let%expect_test "*/> inside () are not special" =
  (* This definitely isn't going to do what you expect it to. *)
  print_parsed "foo > (a > b > *)";
  [%expect {| ((descendants (string foo)) (children (one_of (* > a b)))) |}]
;;

(* Edge cases *)

let%expect_test "Leading >" =
  print_parsed "> a";
  [%expect {| ((children (string a))) |}]
;;

let%expect_test "Trailing >" =
  print_parsed "foo >";
  [%expect {|
    ((descendants (string foo))
     (descendants (string >))) |}]
;;

let%expect_test "Adjacent >" =
  (* Should maybe be a parse error? *)
  print_parsed "foo > > bar";
  [%expect
    {|
    ((descendants (string foo))
     (children    (string >))
     (descendants (string bar))) |}]
;;

let%expect_test "*/> inside () are not special" =
  (* This definitely isn't going to do what you expect it to. *)
  print_parsed "foo > ( a > b > * )";
  [%expect {|
    ((descendants (string foo)) (children (one_of (* > a b)))) |}]
;;
