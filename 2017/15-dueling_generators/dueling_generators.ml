open Core;; 
open OUnit;; 

(* lazy generator for the infinite sequence given by the startValue, factor 
 * and an optional filter predicate *)
let generator ?filter factor startValue = 
  let base_sequence = Batteries.LazyList.drop 1 (Batteries.LazyList.from_loop startValue (fun n -> (n, (n*factor) % 2147483647))) in
  match filter with
    | None -> base_sequence
    | Some pred -> Batteries.LazyList.filter pred base_sequence;;

(* generator for sequence A *)
let generatorA ?filter startValue = generator ?filter 16807 startValue;; 

(* generator for sequence B *)
let generatorB ?filter startValue = generator ?filter 48271 startValue;; 

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
  Batteries.LazyList.fold_left (fun accu (a,b) -> if sameLowest16 a b then accu + 1 else accu) 0 zipped;; 

(* UNIT TESTS *)
let testGeneratorA test_ctxt = assert_equal 
  [1092455; 1181022009; 245556042; 1744312007; 1352636452 ] 
  (Batteries.LazyList.to_list(Batteries.LazyList.take 5 (generatorA 65)));;  

let testGeneratorB test_ctxt = assert_equal 
  [430625591; 1233683848; 1431495498; 137874439; 285222916]
  (Batteries.LazyList.to_list(Batteries.LazyList.take 5 (generatorB 8921)));;  

let testGeneratorAWithFilter test_ctxt = 
  let genA = generatorA ~filter:(fun x -> phys_equal 0 (x % 4)) 65 in
  assert_equal
  [1352636452; 1992081072; 530830436; 1980017072; 740335192]
  (Batteries.LazyList.to_list(Batteries.LazyList.take 5 genA))

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

let testSolvePartII test_ctxt = 
  let genA = generatorA ~filter:(fun x -> phys_equal 0 (x % 4)) 634 in
  let genB = generatorB ~filter:(fun x -> phys_equal 0 (x % 8)) 301 in
  let solution = judge 5000000 genA genB in
  let _ = print_int solution in 
  assert_equal 294 solution;; 

(* Main program: run unit tests (including solutions) *)
let suite =
"suite">:::
 ["generator A, first 5 values">:: testGeneratorA;
  "generator A with filter, first 5 values">:: testGeneratorAWithFilter;
  "generator B, first 5 values">:: testGeneratorB;
  "judge A B, first 5 rounds">:: testJudge;
  "solve part I">:: testSolvePartI;
  "solve part II">:: testSolvePartII]
;;

let _ = run_test_tt_main suite
;;
