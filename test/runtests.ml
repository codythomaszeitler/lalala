open OUnit2
open Lalala.Apexlexer
open Lalala.Apexparser
open Lexing
open Lalala.Ast
open Lalala.Location
open Lalala.ApexModifier
open Lalala.ApexIdentifier
open Lalala.ApexAnnotation
open Lalala.Expr

let suite =
  "RunTests"
  >::: [
         ( "parse apex class definition empty ast" >:: fun _ ->
           let buffer = from_string "@TestVisible public class TestClass {}" in
           let ast = compilationUnit read_token buffer in
           assert_equal ~printer:to_string
             (ApexClassDeclaration
                ( no_loc,
                  Some (ApexAnnotation (no_loc, "TestVisible")),
                  [ Public no_loc ],
                  ApexIdentifier (no_loc, "TestClass"),
                  [] ))
             ast );
         ( "parse apex class definition empty ast with different name"
         >:: fun _ ->
           let buffer = from_string "public class AnotherTestClass {}" in
           let ast = compilationUnit read_token buffer in
           assert_equal
             (ApexClassDeclaration
                ( no_loc,
                  None,
                  [ Public no_loc ],
                  ApexIdentifier (no_loc, "AnotherTestClass"),
                  [] ))
             ast );
         ( "parse apex class definition with no modifier" >:: fun _ ->
           let buffer = from_string "class AnotherTestClass {}" in
           let ast = compilationUnit read_token buffer in
           assert_equal
             (ApexClassDeclaration
                ( no_loc,
                  None,
                  [],
                  ApexIdentifier (no_loc, "AnotherTestClass"),
                  [] ))
             ast );
         ( "parse apex class definition with variable declaration in it"
         >:: fun _ ->
           let buffer =
             from_string
               "public class AnotherTestClass { public int a; public int \
                getA() {return a;} }"
           in
           let ast = compilationUnit read_token buffer in
           assert_equal ~printer:to_string
             (ApexClassDeclaration
                ( no_loc,
                  None,
                  [ Public no_loc ],
                  ApexIdentifier (no_loc, "AnotherTestClass"),
                  [
                    ApexFieldDeclaration
                      ( no_loc,
                        None,
                        [ Public no_loc ],
                        ApexType (no_loc, "int"),
                        [
                          ApexVariableDecl (no_loc, ApexIdentifier (no_loc, "a"));
                        ] );
                    ApexMethodDeclaration
                      ( no_loc,
                        None,
                        [ Public no_loc ],
                        ApexType (no_loc, "int"),
                        ApexIdentifier (no_loc, "getA"),
                        [
                          ApexReturnStmt
                            (no_loc, Id (no_loc, ApexIdentifier (no_loc, "a")));
                        ] );
                  ] ))
             ast );
         ( "it should be able to parse an apex test class with an empty test \
            method"
         >:: fun _ ->
           let buffer =
             from_string
               "@IsTest private class TestClass { @TestMethod private void \
                testMethod() {} }"
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
                        [] );
                  ] ))
             ast );
        (* So what is interesting here now is that we should be able to construct a java tree here *)
         StmtTest.suite;
         ApexTypeTest.suite;
         ExprTest.suite;
         LocationTest.suite;
         OperatorTest.suite;
         ApexModifierTest.suite;
         ApexDeclTest.suite;
         ApexIdentifierTest.suite;
         ApexVariableDeclTest.suite;
         ApexLocalVarDeclTest.suite;
         AstTest.suite;
         TranspilerTest.suite;
       ]

let _ = run_test_tt_main suite
