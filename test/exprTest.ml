open OUnit2
open Lalala.Location
open Lalala.Expr
open Lalala.ApexLiteral

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
             "(Primary\n\
             \  apexLiteral=(IntegerLiteral {int=100; loc=(Location)}); \
              loc=(Location))"
             (Buffer.contents buffer) );
       ]
