open Core;; 
(*open Core.List ;;*)
open OUnit;; 
(*open Batteries.LazyList;;*)
(*open Batteries;;*)

(* lazy generator for the infinite sequence given by the startValue and factor *)
let generator factor startValue = 
  Batteries.LazyList.drop 1 (Batteries.LazyList.from_loop startValue (fun n -> (n, (n*factor) % 2147483647)));;

(* generator for sequence A *)
let generatorA startValue = generator 16807 startValue;; 

(* generator for sequence B *)
let generatorB startValue = generator 48271 startValue;; 

(* compare lowest 16 bits; return true if those are equal, false otherwise *)
let sameLowest16 a b = 
  let a' = a land (int_of_float(2.0 ** 16.0) - 1) in
  let b' = b land (int_of_float(2.0 ** 16.0) - 1) in
  phys_equal a' b';;

(* judge: compute the number of equalities of genA and genB (lowest 16 bits) for num_rounds *) 
let judge num_rounds genA genB = 
  let lst_a = Batteries.LazyList.take num_rounds genA in
  let lst_b = Batteries.LazyList.take num_rounds genB in
  let zipped = Batteries.LazyList.combine lst_a lst_b in
  (*List.fold zipped ~init:0 ~f:(fun accu (a,b) -> if sameLowest16 a b then accu + 1 else accu);;*)
  Batteries.LazyList.fold_left (fun accu (a,b) -> if sameLowest16 a b then accu + 1 else accu) 0 zipped;; 

(* UNIT TESTS *)
let testGeneratorA test_ctxt = assert_equal 
  [1092455; 1181022009; 245556042; 1744312007; 1352636452 ] 
  (Batteries.LazyList.to_list(Batteries.LazyList.take 5 (generatorA 65)));;  

let testGeneratorB test_ctxt = assert_equal 
  [430625591; 1233683848; 1431495498; 137874439; 285222916]
  (Batteries.LazyList.to_list(Batteries.LazyList.take 5 (generatorB 8921)));;  

let testJudge test_ctxt = 
  let genA = generatorA 65 in
  let genB = generatorB 8921 in
    assert_equal 1 (judge 5 genA genB);;

let testSolvePartI test_ctxt = 
  let genA = generatorA 634 in
  let genB = generatorB 301 in
  let solution = judge 40000000 genA genB in
  let _ = print_int solution in 
  assert_equal 573 solution;; 

(* unit tests *)
let suite =
"suite">:::
 ["generator A, first 5 values">:: testGeneratorA;
  "generator B, first 5 values">:: testGeneratorB;
  "judge A B, first 5 rounds">:: testJudge;
  "solve part I">:: testSolvePartI]
;;

let _ = run_test_tt_main suite
;;
