open OUnit2

let suite =
  "Utility"
  >::: [
         ( "it should be able to get all Some of any type" >:: fun _ ->
           let list = [ Some "a"; None; Some "b" ] in
           let filtered = Lalala.Utility.filter_optionals list in
           assert_equal [ "a"; "b" ] filtered );
         ( "it should be able to return an empty list when given an empty list"
         >:: fun _ ->
           let list = [] in
           let filtered = Lalala.Utility.filter_optionals list in
           assert_equal [] filtered );
         ( "it should be able to return an empty list if given all nones"
         >:: fun _ ->
           let list = [ None; None; None; None; None ] in
           let filtered = Lalala.Utility.filter_optionals list in
           assert_equal [] filtered );
       ]
