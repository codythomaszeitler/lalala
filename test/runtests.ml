open OUnit2
open Lalala.Apexlexer
open Lalala.Apexparser
open Lexing
open Lalala.Ast

let tests =
  "test suite for apex lexer"
  >::: [
         (* ( "parse integer" >:: fun _ ->
              let buffer = from_string "2" in
              let expected = INT 2 in
              let actual = read_token buffer in
              assert_equal expected actual );
            ( "parse add sign" >:: fun _ ->
              let buffer = from_string "+" in
              let expected = PLUS in
              let actual = read_token buffer in
              assert_equal expected actual );
            ( "parse multiplication sign" >:: fun _ ->
              let buffer = from_string "*" in
              let expected = MULT in
              let actual = read_token buffer in
              assert_equal expected actual );
            ( "parse abstract token" >:: fun _ ->
              let buffer = from_string "abstract" in
              let expected = ABSTRACT in
              let actual = read_token buffer in
              assert_equal expected actual );
            ( "parse after token" >:: fun _ ->
              let buffer = from_string "after" in
              let expected = AFTER in
              let actual = read_token buffer in
              assert_equal expected actual );
            ( "parse addition ast" >:: fun _ ->
              let buffer = from_string "2+3" in
              let ast = main read_token buffer in
              assert_equal (Binop (Add, Int 2, Int 3)) ast );
            ( "parse subtraction ast" >:: fun _ ->
              let buffer = from_string "2*3" in
              let ast = main read_token buffer in
              assert_equal (Binop (Mult, Int 2, Int 3)) ast ); *)
         ( "parse apex class definition empty ast" >:: fun _ ->
           let buffer = from_string "public class TestClass {}" in
           let ast = compilationUnit read_token buffer in
           assert_equal
             (TypeDecl (Public, ClassDeclaration (Identifier "TestClass", [])))
             ast );
         ( "parse apex class definition empty ast with different name"
         >:: fun _ ->
           let buffer = from_string "public class AnotherTestClass {}" in
           let ast = compilationUnit read_token buffer in
           assert_equal
             (TypeDecl
                (Public, ClassDeclaration (Identifier "AnotherTestClass", [])))
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
                ( Public,
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
       ]

let _ = run_test_tt_main tests
