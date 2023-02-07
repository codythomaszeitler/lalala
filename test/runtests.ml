open OUnit2
open Lalala.Apexlexer
open Lalala.Apexparser
open Lexing
open Lalala.Ast

let tests =
  "test suite for apex lexer"
  >::: [
         ( "parse integer" >:: fun _ ->
           let buffer = from_string "2" in
           let expected = INT 2 in
           let actual = read_token buffer in
           assert_equal expected actual );
         ( "parse add sign" >:: fun _ ->
           let buffer = from_string "+" in
           let expected = PLUS in
           let actual = read_token buffer in
           assert_equal expected actual );
         ( "parse minus sign" >:: fun _ ->
           let buffer = from_string "-" in
           let expected = MINUS in
           let actual = read_token buffer in
           assert_equal expected actual );
         ( "parse multiplication sign" >:: fun _ ->
           let buffer = from_string "*" in
           let expected = MULT in
           let actual = read_token buffer in
           assert_equal expected actual );
         ( "parse division sign" >:: fun _ ->
           let buffer = from_string "/" in
           let expected = DIV in
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
           let buffer = from_string "2\n" in
           let ast = main read_token buffer in
           assert_equal (Int 2) ast );
       ]

let _ = run_test_tt_main tests
