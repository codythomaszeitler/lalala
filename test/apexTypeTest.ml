open OUnit2
open Lalala.ApexType

let suite =
  "test suite for apex type test"
  >::: [
         ( "it should be able to tostring an apex type" >:: fun _ ->
           let apexType = ApexType (Identifier "int") in
           assert_equal
             ~printer:(fun x -> x)
             "(ApexType\n  (Identifier\n    (\"int\")))" (to_string apexType 0)
         );
       ]
