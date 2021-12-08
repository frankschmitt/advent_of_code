pub fn solve() {
    let filename = "a06_lanternfish/example_input.txt";
    //let filename = "a06_lanternfish/input.txt";
    let fishes: Vec<i64> = crate::helpers::read_int_comma_separated_list((&filename).to_string());

    let mut fish_counts: Vec<u128> = vec![0, 0, 0, 0, 0, 0, 0, 0, 0];
    let mut result1: u128 = 0;
    for f in fishes {
      fish_counts[f as usize] += 1;
    }
    // part I
    for i in 1..=256 {
        let adult_fish_count = fish_counts[0];
        for j in 1 ..=8 {
          // move fishes one forward
          fish_counts[j-1] = fish_counts[j];
        }
        fish_counts[6] += adult_fish_count; // wrap around
        fish_counts[8] += adult_fish_count; // add new fishes
        if i == 80 {
          result1 = fish_counts.iter().fold(0, |acc, cnt| acc + cnt);
        }
        println!("day {}: {:?}", i, fish_counts);
    }
    let result2 = fish_counts.iter().fold(0, |acc, cnt| acc + cnt);

    // part II - we cannot solve this with a vector (it would become way too large)
    // idea: instead of storing the whole vector, store only (age, count) pairs; in each iteration, (age, count) becomes (age-1, count)
    println!("06 - lanternfish: {} {}", result1, result2);
}