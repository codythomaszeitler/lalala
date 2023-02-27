open OUnit2

let suite =
  "ApexType"
  >::: [
         ( "it should be able to pretty print an apex type" >:: fun _ ->
           let buffer = Buffer.create 5 in
           let formatter = Format.formatter_of_buffer buffer in
           let apexType =
             Lalala.ApexType.ApexType
               ( Lalala.Location.no_loc,
                 Lalala.ApexIdentifier.ApexIdentifier
                   (Lalala.Location.no_loc, "int") )
           in
           Lalala.ApexType.pr_apex_type formatter apexType;
           Format.pp_print_flush formatter ();
           assert_equal
             ~printer:(fun x -> x)
             "(ApexType\n\
             \  {identifier=(ApexIdentifier {name=\"int\" loc=(Location)}); \
              loc=(Location)})"
             (Buffer.contents buffer) );
       ]
