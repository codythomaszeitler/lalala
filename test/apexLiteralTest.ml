open OUnit2
open Lalala.Location
open Lalala.ApexLiteral

let suite =
  "ApexLiteral"
  >::: [
         ( "it should be able to pretty print an integer literal" >:: fun _ ->
           let buffer = Buffer.create 5 in
           let formatter = Format.formatter_of_buffer buffer in
           let integerLiteral = IntegerLiteral (no_loc, 100) in
           pr_apex_literal formatter integerLiteral;
           Format.pp_print_flush formatter ();
           assert_equal
             ~printer:(fun x -> x)
             "(IntegerLiteral{\n  int=100;\n  loc=(Location)})"
             (Buffer.contents buffer) );
         ( "it should be able to pretty print a long literal" >:: fun _ ->
           let buffer = Buffer.create 5 in
           let formatter = Format.formatter_of_buffer buffer in
           let longLiteral = LongLiteral (no_loc, 100) in
           pr_apex_literal formatter longLiteral;
           Format.pp_print_flush formatter ();
           assert_equal
             ~printer:(fun x -> x)
             "(LongLiteral{\n  long=100;\n  loc=(Location)})"
             (Buffer.contents buffer) );
         ( "it should be able to pretty print a string literal" >:: fun _ ->
           let buffer = Buffer.create 5 in
           let formatter = Format.formatter_of_buffer buffer in
           let stringLiteral = StringLiteral (no_loc, "abc") in
           pr_apex_literal formatter stringLiteral;
           Format.pp_print_flush formatter ();
           assert_equal
             ~printer:(fun x -> x)
             "(StringLiteral{\n  string=\"abc\";\n  loc=(Location)})"
             (Buffer.contents buffer) );
         ( "it should be able to pretty print a boolean literal" >:: fun _ ->
           let buffer = Buffer.create 5 in
           let formatter = Format.formatter_of_buffer buffer in
           let booleanLiteral = BooleanLiteral (no_loc, true) in
           pr_apex_literal formatter booleanLiteral;
           Format.pp_print_flush formatter ();
           assert_equal
             ~printer:(fun x -> x)
             "(BooleanLiteral{\n  bool=true;\n  loc=(Location)})"
             (Buffer.contents buffer) );
         ( "it should be able to pretty print a null literal" >:: fun _ ->
           let buffer = Buffer.create 5 in
           let formatter = Format.formatter_of_buffer buffer in
           let booleanLiteral = NullLiteral no_loc in
           pr_apex_literal formatter booleanLiteral;
           Format.pp_print_flush formatter ();
           assert_equal
             ~printer:(fun x -> x)
             "(NullLiteral{\n  loc=(Location)})" (Buffer.contents buffer) );
       ]
