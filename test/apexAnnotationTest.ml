open OUnit2

let suite =
  "ApexAnnotation"
  >::: [
         (* ( "it should be able to convert from string into apex annotation"
         >:: fun _ ->
           let raw = "IsTest" in
           let loc = Lalala.Location.no_loc in
           assert_equal IsTest loc (from_string low raw) ); *)
       ]
