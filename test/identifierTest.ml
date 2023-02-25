open OUnit2
open Lalala.Identifier

let suite =
  "Identifier"
  >::: [
         ( "it should be able to pretty print an identifier" >:: fun _ ->
           let buffer = Buffer.create 5 in
           let formatter = Format.formatter_of_buffer buffer in
           let identifier = Lalala.Identifier.create "testIdentifier" in
           pr_identifer formatter identifier;
           Format.pp_print_flush formatter ();
           assert_equal
             ~printer:(fun x -> x)
             "(Identifier { name=\"testIdentifier\" })" (Buffer.contents buffer) );
       ]
