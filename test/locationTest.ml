open OUnit2
open Lalala.Location

let suite =
  "Location"
  >::: [
         ( "it should be able to pretty print a location" >:: fun _ ->
           let buffer = Buffer.create 5 in
           let formatter = Format.formatter_of_buffer buffer in
           let location = create 1 2 3 in
           pr_location formatter location;
           Format.pp_print_newline formatter ();
           assert_equal
             ~printer:(fun x -> x)
             "(Location{line=1,row=2,column=3})\n"
             (Buffer.contents buffer) );
         ( "it should be able to pretty print a none location" >:: fun _ ->
           let buffer = Buffer.create 5 in
           let formatter = Format.formatter_of_buffer buffer in
           pr_location formatter no_loc;
           Format.pp_print_flush formatter ();
           assert_equal
             ~printer:(fun x -> x)
             "(Location)" (Buffer.contents buffer) );
       ]
