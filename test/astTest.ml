open OUnit2
open Lalala.Location
open Lalala.Ast
open Lalala.ApexIdentifier

let suite =
  "ApexClassDeclaration"
  >::: [
         ( "it should be able to pretty print an apex class decl" >:: fun _ ->
           let buffer = Buffer.create 5 in
           let formatter = Format.formatter_of_buffer buffer in
           let ast =
             ApexClassDeclaration
               ( no_loc,
                 None,
                 [],
                 ApexIdentifier (no_loc, "AnotherTestClass"),
                 [] )
           in
           pr_compilation_unit formatter ast;
           Format.pp_print_flush formatter ();
           assert_equal
             ~printer:(fun x -> x)
             "(ApexClassDeclaration{\n\
             \  annotation=\n\
             \    \n\
             \  modis=\n\
             \    []\n\
             \  id=\n\
             \    (ApexIdentifier{\n\
             \      name=\n\
             \        \"AnotherTestClass\"\n\
             \      loc=\n\
             \        (Location)})\n\
             \  class_body_decls=\n\
             \    []\n\
             \  location=\n\
             \    (Location)})" (Buffer.contents buffer) );
         ( "it should be able to tell if the apex class is a test class when \
            it is a test class"
         >:: fun _ ->
           let apex =
             ApexClassDeclaration
               ( no_loc,
                 Some (ApexAnnotation (no_loc, "IsTest")),
                 [],
                 ApexIdentifier (no_loc, "TestClass"),
                 [] )
           in
           assert_equal true (is_test_class apex) );
         ( "it should be able to tell if the apex class is a test class when \
            it is not a test class"
         >:: fun _ ->
           let apex =
             ApexClassDeclaration
               (no_loc, None, [], ApexIdentifier (no_loc, "NotATestClass"), [])
           in
           assert_equal false (is_test_class apex) );
       ]
