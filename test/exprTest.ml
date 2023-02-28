open OUnit2
open Lalala.Location
open Lalala.Expr
open Lalala.ApexLiteral
open Lalala.Operator

let suite =
  "Expr"
  >::: [
         ( "it should be able to pretty print an integer literal" >:: fun _ ->
           let buffer = Buffer.create 5 in
           let formatter = Format.formatter_of_buffer buffer in
           let primaryExpr = Primary (no_loc, IntegerLiteral (no_loc, 100)) in
           pr_expr formatter primaryExpr;
           Format.pp_print_flush formatter ();
           assert_equal
             ~printer:(fun x -> x)
             "(Primary{\n\
             \  apexLiteral=\n\
             \    (IntegerLiteral{\n\
             \      int=100;\n\
             \      loc=(Location)});\n\
             \  loc=(Location)})" (Buffer.contents buffer) );
         ( "it should be able to pretty print a binary expression" >:: fun _ ->
           let buffer = Buffer.create 5 in
           let formatter = Format.formatter_of_buffer buffer in
           let left = Primary (no_loc, IntegerLiteral (no_loc, 100)) in
           let right = Primary (no_loc, IntegerLiteral (no_loc, 200)) in
           let op = Add no_loc in
           let binary = Binary (no_loc, left, op, right) in
           pr_expr formatter binary;
           Format.pp_print_flush formatter ();
           assert_equal
             ~printer:(fun x -> x)
             "(Binary{\n\
             \  left=\n\
             \    (Primary{\n\
             \      apexLiteral=\n\
             \        (IntegerLiteral{\n\
             \          int=100;\n\
             \          loc=(Location)});\n\
             \      loc=(Location)});\n\
             \  op=\n\
             \    (Add{\n\
             \      loc=(Location)});\n\
             \  right=\n\
             \    (Primary{\n\
             \      apexLiteral=\n\
             \        (IntegerLiteral{\n\
             \          int=200;\n\
             \          loc=(Location)});\n\
             \      loc=(Location)});\n\
             \  loc=\n\
             \    (Location)})" (Buffer.contents buffer) );
       ]
