open Core;; 
(*open Core.List ;;*)
open OUnit;; 
(*open Batteries.LazyList;;*)
(*open Batteries;;*)

let unity x = x ;;


(* 

The generators both work on the same principle. To create its next value, a generator will take the previous value it produced, multiply it by a factor (generator A uses 16807; generator B uses 48271), and then keep the remainder of dividing that resulting product by 2147483647. That final remainder is the value it produces next.
 *
 * for start values 65 and 8921:
  *
   1092455   430625591
1181022009  1233683848
 245556042  1431495498
1744312007   137874439
1352636452   285222916
 * *)

(*let rec generator factor startValue = 
  let nextValue =  (factor * startValue) % 2147483647 in
  [nextValue] @ generator(factor, nextValue) ;; (*[1092455];;*)
*)
let generator factor startValue = 
  Batteries.LazyList.drop 1 (Batteries.LazyList.from_loop startValue (fun n -> (n, (n*factor) % 2147483647)));;

let generatorA startValue = generator 16807 startValue;; 
let generatorB startValue = generator 48271 startValue;; 

let testGeneratorA test_ctxt = assert_equal 
  [1092455; 1181022009; 245556042; 1744312007; 1352636452 ] 
  (Batteries.LazyList.to_list(Batteries.LazyList.take 5 (generatorA 65)));;  

let testGeneratorB test_ctxt = assert_equal 
  [430625591; 1233683848; 1431495498; 137874439; 285222916]
  (Batteries.LazyList.to_list(Batteries.LazyList.take 5 (generatorB 8921)));;  


(* Name the test cases and group them together *)
let suite =
"suite">:::
 ["generator A, first 5 values">:: testGeneratorA;
  "generator B, first 5 values">:: testGeneratorB]
;;

let _ = run_test_tt_main suite
;;
