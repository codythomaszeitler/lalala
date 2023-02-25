open OUnit2
open Lalala.ApexIdentifier

let suite =
  "Identifier"
  >::: [
         ( "it should be able to pretty print an identifier" >:: fun _ ->
           let buffer = Buffer.create 5 in
           let formatter = Format.formatter_of_buffer buffer in
           let identifier = Lalala.ApexIdentifier.create "testIdentifier" in
           pr_identifer formatter identifier;
           Format.pp_print_flush formatter ();
           assert_equal
             ~printer:(fun x -> x)
             "(ApexIdentifier\n  {name=\"testIdentifier\" loc=(Location)})"
             (Buffer.contents buffer) );
       ]
