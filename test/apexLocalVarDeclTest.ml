open OUnit2
open Lalala.ApexLocalVarDecl
open Lalala.ApexModifier
open Lalala.ApexType
open Lalala.ApexVariableDecl
open Lalala.ApexIdentifier
open Lalala.Location

let suite =
  "test suite for local var decl ast"
  >::: [
         ( "it should be able to tostring a local var decl" >:: fun _ ->
           let buffer = Buffer.create 5 in
           let formatter = Format.formatter_of_buffer buffer in
           let astNode =
             ApexLocalVarDecl
               ( no_loc,
                 Public no_loc,
                 ApexType (no_loc, ApexIdentifier (no_loc, "int")),
                 [
                   ApexVariableDecl (no_loc, ApexIdentifier (no_loc, "a"));
                   ApexVariableDecl (no_loc, ApexIdentifier (no_loc, "b"));
                   ApexVariableDecl (no_loc, ApexIdentifier (no_loc, "c"));
                 ] )
           in
           pr_local_var_decl formatter astNode;
           Format.pp_print_flush formatter ();
           assert_equal
             ~printer:(fun x -> x)
             "(ApexLocalVarDecl{\n\
             \                  modifier=(Public{\n\
             \                                  loc=(Location)};\n\
             \                  apex_type=(ApexType{\n\
             \                                     identifier=(ApexIdentifier{\n\
             \                                                               \
              name=\"int\"\n\
             \                                                               \
              loc=(Location)});\n\
             \                                     loc=(Location)});\n\
             \                  variable_decl=\n\
             \                  [(ApexVariableDecl{\n\
             \                                    id=(ApexIdentifier{\n\
             \                                                      name=\"a\"\n\
             \                                                      \
              loc=(Location)});\n\
             \                                    loc=(Location)})\n\
             \                  (ApexVariableDecl{\n\
             \                                   id=(ApexIdentifier{\n\
             \                                                     name=\"b\"\n\
             \                                                     \
              loc=(Location)});\n\
             \                                   loc=(Location)})\n\
             \                  (ApexVariableDecl{\n\
             \                                   id=(ApexIdentifier{\n\
             \                                                     name=\"c\"\n\
             \                                                     \
              loc=(Location)});\n\
             \                                   loc=(Location)})];\n\
             \                  location=(Location);})" (Buffer.contents buffer)
         );
       ]
