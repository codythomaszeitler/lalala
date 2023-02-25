open OUnit2

let suite =
  "ApexVariableDecl"
  >::: [
         ( "it should be able to pretty print a variable decl" >:: fun _ ->
           let buffer = Buffer.create 5 in
           let formatter = Format.formatter_of_buffer buffer in
           let variableDecl =
             Lalala.ApexVariableDecl.create (Lalala.ApexIdentifier.create "a")
           in
           Lalala.ApexVariableDecl.pr_variable_decl formatter variableDecl;
           Format.pp_print_flush formatter ();
           assert_equal
             ~printer:(fun x -> x)
             "(ApexVariableDecl\n  {id=(ApexIdentifier {name=\"a\" loc=(Location)}); \
              loc=(Location)})"
             (Buffer.contents buffer) );
       ]
