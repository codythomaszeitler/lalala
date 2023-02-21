open OUnit2

let suite =
  "test suite for apex type test"
  >::: [
         ( "it should be able to tostring an apex type" >:: fun _ ->
           let apexType = Lalala.ApexType.ApexType (Identifier "int") in
           assert_equal
             ~printer:(fun x -> x)
             "(ApexType\n  (Identifier\n    (\"int\")))" (Lalala.ApexType.to_string apexType 0)
         );
       ]