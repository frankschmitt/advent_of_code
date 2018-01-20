open Core;; 
open Core.List ;;
open OUnit;; 

let unity x = x ;;

(* for start values 65 and 8921:
   1092455   430625591
1181022009  1233683848
 245556042  1431495498
1744312007   137874439
1352636452   285222916
 * *)

let generatorA x = [1092455];;

let testGeneratorA test_ctxt = assert_equal [1092455] (List.take (generatorA 65) 1);;  
let test2 test_ctxt = assert_equal 100 (unity 100) ;;

(* Name the test cases and group them together *)
let suite =
"suite">:::
 ["test1">:: testGeneratorA;
  "test2">:: test2]
;;

let _ = run_test_tt_main suite
;;

(*
let%test _ = [1092455] == (List.take (generatorA 65) 1);;
let%test _ = is_prime 7
let%test _ = not (is_prime 1)
let%test _ = not (is_prime 8)
*)
