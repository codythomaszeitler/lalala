open OUnit2
open Lalala.ApexTypeName
open Lalala.Location

let suite =
  "ApexType"
  >::: [
         ( "it should be able to pretty print an apex type" >:: fun _ ->
           let buffer = Buffer.create 5 in
           let formatter = Format.formatter_of_buffer buffer in
           let apexType = ApexTypeName (no_loc, "int") in
           pr_apex_type formatter apexType;
           Format.pp_print_flush formatter ();
           assert_equal
             ~printer:(fun x -> x)
             "(ApexTypeName{\n  identifier=\n    int;\n  loc=(Location)})"
             (Buffer.contents buffer) );
       ]
