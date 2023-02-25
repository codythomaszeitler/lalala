open OUnit2

let suite =
  "Modifier"
  >::: [
         ( "it should be able to pretty print a modifier" >:: fun _ ->
           let buffer = Buffer.create 5 in
           let formatter = Format.formatter_of_buffer buffer in
           let modifier = Lalala.Modifier.create Public in
           Lalala.Modifier.pr_modifier formatter modifier;
           Format.pp_print_flush formatter ();
           assert_equal
             ~printer:(fun x -> x)
             "(Modifier\n  {modifier_type=\"public\"; loc=(Location)}"
             (Buffer.contents buffer) );
       ]
