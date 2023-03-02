open OUnit2
open Lalala.Apexlexer
open Lalala.Apexparser
open Lexing
open Lalala.Ast
open Lalala.Location
open Lalala.ApexModifier
open Lalala.ApexIdentifier
open Lalala.ApexAnnotation

let suite =
  "ApexParser"
  >::: [
         ( "it should be able to parse apex test class that asserts two \
            integers are equal"
         >:: fun _ ->
           let buffer =
             from_string
               " @IsTest private class TestClass {\n\
               \  @TestMethod private void testMethod() {\n\
               \    System.assertEquals(2, 3);\n\
               \  }\n\
               \ }"
           in
           let ast = compilationUnit read_token buffer in
           assert_equal ~printer:to_string
             (ApexClassDeclaration
                ( no_loc,
                  Some (ApexAnnotation (no_loc, "IsTest")),
                  [ Private no_loc ],
                  ApexIdentifier (no_loc, "TestClass"),
                  [
                    ApexMethodDeclaration
                      ( no_loc,
                        Some (ApexAnnotation (no_loc, "TestMethod")),
                        [ Private no_loc ],
                        ApexType (no_loc, "void"),
                        ApexIdentifier (no_loc, "testMethod"),
                        [
                          ApexExprStmt
                            ( no_loc,
                              ApexMethodCall
                                ( no_loc,
                                  ApexIdentifier (no_loc, "System.assertEquals"),
                                  [
                                    IntegerLiteral (no_loc, 2);
                                    IntegerLiteral (no_loc, 3);
                                  ] ) );
                        ] );
                  ] ))
             ast );
       ]
