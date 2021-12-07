// part I is easy-peasy - just take the median and compute the sum of distances from it
pub fn solve() {
    //let filename = "a07_the_treachery_of_whales/example_input.txt";
    let filename = "a07_the_treachery_of_whales/input.txt";
    let mut v = crate::helpers::read_int_comma_separated_list((&filename).to_string());
    // println!("{:?}", v);
    v.sort();
    let mid = v.len() / 2;
    let median = v[mid];
    let result1 = v.iter().fold(0, |acc, val| acc + (val - median).abs());
    let result2 = -1;
    println!("a07 - the treachery of whales: {} {}", result1, result2);
}