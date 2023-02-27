open OUnit2
open Lalala.Location
open Lalala.Operator

let suite =
  "Operator"
  >::: [
         ( "it should be able to pretty print an operator" >:: fun _ ->
           let buffer = Buffer.create 5 in
           let formatter = Format.formatter_of_buffer buffer in
           let operator : operator = Add no_loc in
           Lalala.Operator.pr_operator formatter operator;
           Format.pp_print_flush formatter ();
           assert_equal
             ~printer:(fun x -> x)
             "(Add\n  {loc=(Location))" (Buffer.contents buffer)
         );
       ]
