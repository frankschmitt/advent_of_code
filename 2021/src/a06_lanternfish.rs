pub fn solve() {
    //let filename = "a06_lanternfish/example_input.txt";
    let filename = "a06_lanternfish/input.txt";
    let mut fishes: Vec<i64> = crate::helpers::read_int_comma_separated_list((&filename).to_string());
    // part 
    for i in 1..=80 {
        let mut new_fishes: Vec<i64> = fishes.iter().filter(|x| **x == 0).map(|_x| 8).collect();
        for j in 0..fishes.len() {
            let new_val = match fishes[j] {
                0 => 6,
                x => x-1
            };
            fishes[j] = new_val;
        }
        fishes.append(&mut new_fishes);
        //println!("After day {} : fishes = {:?}", i, fishes);
    }
    let result1 = fishes.len();
    // part II - we cannot solve this with a vector (it would become way too large)
    // idea: instead of storing the whole vector, store only (age, count) pairs; in each iteration, (age, count) becomes (age-1, count)
    let result2 = -1;
    println!("06 - lanternfish: {} {}", result1, result2);
}