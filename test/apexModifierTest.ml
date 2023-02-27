open OUnit2
open Lalala.ApexModifier
open Lalala.Location

let suite =
  "Modifier"
  >::: [
         ( "it should be able to pretty print a modifier" >:: fun _ ->
           let buffer = Buffer.create 5 in
           let formatter = Format.formatter_of_buffer buffer in
           let modifier = Public no_loc in
           Lalala.ApexModifier.pr_modifier formatter modifier;
           Format.pp_print_flush formatter ();
           assert_equal
             ~printer:(fun x -> x)
             "(Modifier\n  {modifier_type=\"public\"; loc=(Location)}"
             (Buffer.contents buffer) );
       ]
