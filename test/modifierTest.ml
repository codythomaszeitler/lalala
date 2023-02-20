open OUnit2

let suite =
  "test suite for variable decl apex ast"
  >::: [
         ( "tostring for the modifiers single arg" >:: fun _ ->
           let modifiers = [ Lalala.Modifier.Public ] in
           assert_equal
             ~printer:(fun x -> x)
             "[\n  (Public)\n]"
             (Lalala.Modifier.to_strings modifiers 0) );
         ( "tostring for the modifiers multiple args" >:: fun _ ->
           let to_string_value =
             Lalala.Modifier.to_strings
               [ Public; Annotation (Identifier "a") ]
               0
           in
           assert_equal
             ~printer:(fun x -> x)
             "[\n\
             \  (Public);\n\
             \  (Annotation\n\
             \    (Identifier\n\
             \      (\"a\")))\n\
              ]"
             to_string_value );
       ]
