open OUnit2
open Lalala.Location
open Lalala.Expr
open Lalala.Operator

let suite =
  "Expr"
  >::: [
         ( "it should be able to pretty print a binary expression" >:: fun _ ->
           let buffer = Buffer.create 5 in
           let formatter = Format.formatter_of_buffer buffer in
           let left = IntegerLiteral (no_loc, 100) in
           let right = IntegerLiteral (no_loc, 200) in
           let op = Add no_loc in
           let binary = Binary (no_loc, left, op, right) in
           pr_expr formatter binary;
           Format.pp_print_flush formatter ();
           assert_equal
             ~printer:(fun x -> x)
             "(Binary{\n\
             \  left=\n\
             \    (IntegerLiteral{\n\
             \      int=100;\n\
             \      loc=(Location)});\n\
             \  op=\n\
             \    (Add{\n\
             \      loc=(Location)});\n\
             \  right=\n\
             \    (IntegerLiteral{\n\
             \      int=200;\n\
             \      loc=(Location)});\n\
             \  loc=\n\
             \    (Location)})" (Buffer.contents buffer) );
         ( "it should be able to pretty print an integer literal" >:: fun _ ->
           let buffer = Buffer.create 5 in
           let formatter = Format.formatter_of_buffer buffer in
           let integerLiteral = IntegerLiteral (no_loc, 100) in
           pr_expr formatter integerLiteral;
           Format.pp_print_flush formatter ();
           assert_equal
             ~printer:(fun x -> x)
             "(IntegerLiteral{\n  int=100;\n  loc=(Location)})"
             (Buffer.contents buffer) );
         ( "it should be able to pretty print a long literal" >:: fun _ ->
           let buffer = Buffer.create 5 in
           let formatter = Format.formatter_of_buffer buffer in
           let longLiteral = LongLiteral (no_loc, 100) in
           pr_expr formatter longLiteral;
           Format.pp_print_flush formatter ();
           assert_equal
             ~printer:(fun x -> x)
             "(LongLiteral{\n  long=100;\n  loc=(Location)})"
             (Buffer.contents buffer) );
         ( "it should be able to pretty print a string literal" >:: fun _ ->
           let buffer = Buffer.create 5 in
           let formatter = Format.formatter_of_buffer buffer in
           let stringLiteral = StringLiteral (no_loc, "abc") in
           pr_expr formatter stringLiteral;
           Format.pp_print_flush formatter ();
           assert_equal
             ~printer:(fun x -> x)
             "(StringLiteral{\n  string=\"abc\";\n  loc=(Location)})"
             (Buffer.contents buffer) );
         ( "it should be able to pretty print a boolean literal" >:: fun _ ->
           let buffer = Buffer.create 5 in
           let formatter = Format.formatter_of_buffer buffer in
           let booleanLiteral = BooleanLiteral (no_loc, true) in
           pr_expr formatter booleanLiteral;
           Format.pp_print_flush formatter ();
           assert_equal
             ~printer:(fun x -> x)
             "(BooleanLiteral{\n  bool=true;\n  loc=(Location)})"
             (Buffer.contents buffer) );
         ( "it should be able to pretty print a null literal" >:: fun _ ->
           let buffer = Buffer.create 5 in
           let formatter = Format.formatter_of_buffer buffer in
           let booleanLiteral = NullLiteral no_loc in
           pr_expr formatter booleanLiteral;
           Format.pp_print_flush formatter ();
           assert_equal
             ~printer:(fun x -> x)
             "(NullLiteral{\n  loc=(Location)})" (Buffer.contents buffer) );
       ]
