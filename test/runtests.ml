open OUnit2
open Lalala.Apexlexer
open Lalala.Apexparser
open Lexing

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
       ]

let _ = run_test_tt_main tests
