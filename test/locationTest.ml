open OUnit2

let suite =
  "Location"
  >::: [
         ( "it should be able to pretty print a location" >:: fun _ ->
           let buffer = Buffer.create 5 in
           let formatter = Format.formatter_of_buffer buffer in
           let identifier = Lalala.Location.create 1 2 3 in
           Lalala.Location.pr_location formatter (Some identifier);
           Format.pp_print_newline formatter ();
           assert_equal
             ~printer:(fun x -> x)
             "(Location\n  {line=1, row=2, column=3})\n"
             (Buffer.contents buffer) );
         ( "it should be able to pretty print a none location" >:: fun _ ->
           let buffer = Buffer.create 5 in
           let formatter = Format.formatter_of_buffer buffer in
           Lalala.Location.pr_location formatter None;
           Format.pp_print_newline formatter ();
           assert_equal
             ~printer:(fun x -> x)
             "(Location)" (Buffer.contents buffer) );
       ]
