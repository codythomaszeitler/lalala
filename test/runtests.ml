open OUnit2
open Lalala.Apexlexer
open Lalala.Apexparser
open Lexing
open Lalala.Ast

let tests =
  "test suite for apex lexer"
  >::: [
         ( "parse apex class definition empty ast" >:: fun _ ->
           let buffer = from_string "public class TestClass {}" in
           let ast = compilationUnit read_token buffer in
           assert_equal
             (TypeDecl
                ([ Public ], ClassDeclaration (Identifier "TestClass", [])))
             ast );
         ( "parse apex class definition empty ast with different name"
         >:: fun _ ->
           let buffer = from_string "public class AnotherTestClass {}" in
           let ast = compilationUnit read_token buffer in
           assert_equal
             (TypeDecl
                ( [ Public ],
                  ClassDeclaration (Identifier "AnotherTestClass", []) ))
             ast );
         ( "parse apex class definition with variable declaration in it"
         >:: fun _ ->
           let buffer =
             from_string
               "public class AnotherTestClass { public int a; public int \
                getA() {return a;} }"
           in
           let ast = compilationUnit read_token buffer in
           assert_equal
             (TypeDecl
                ( [ Public ],
                  ClassDeclaration
                    ( Identifier "AnotherTestClass",
                      [
                        ClassBodyDeclaration
                          ( Public,
                            FieldDeclaration
                              ( ApexType (Identifier "int"),
                                [ VariableDecl (Identifier "a") ] ) );
                        ClassBodyDeclaration
                          ( Public,
                            MethodDeclaration
                              ( ApexType (Identifier "int"),
                                Identifier "getA",
                                [ ReturnStmt (Primary (Identifier "a")) ] ) );
                      ] ) ))
             ast );
         (* ( "parse apex class definition with an annotation at the top"
            >:: fun _ ->
              let buffer = from_string "@IsTest public class AppTest {}" in
              let ast = compilationUnit read_token buffer in
              assert_equal
                (TypeDecl
                   ( [ Annotation (Identifier "IsTest"); Public ],
                     ClassDeclaration (Identifier "AnotherTestClass", []) ))
                ast ); *)
         ( "tostring for the modifiers single arg" >:: fun _ ->
           let modifiers = [ Public ] in
           assert_equal "[Public]" (to_string_modifiers modifiers) );
         ( "tostring for the modifiers multiple args" >:: fun _ ->
           let to_string_value =
             to_string_modifiers [ Public; Annotation (Identifier "a") ]
           in
           (*So that ; right there is ignoring the value that we are working with? *)
           print_string to_string_value;
           assert_equal "[Public, Annotation(Identifier(\"a\"))]"
             to_string_value );
       ]

let _ = run_test_tt_main tests
