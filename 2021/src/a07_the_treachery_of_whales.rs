use std::cmp::min;

pub fn solve() {
    //let filename = "a07_the_treachery_of_whales/example_input.txt";
    let filename = "a07_the_treachery_of_whales/input.txt";
    let mut v = crate::helpers::read_int_comma_separated_list((&filename).to_string());
    // println!("{:?}", v);
    v.sort();
    // part I is easy-peasy - just take the median (it minimizes the sum of distances) and compute the sum of distances from it
    let mid = v.len() / 2;
    let median = v[mid];
    let result1 = v.iter().fold(0, |acc, val| acc + (val - median).abs());
    // part II: take the mean (since it minimizes the sum of squared distances), compute the distance and the cost 
    //          the cost is the sum from i=1,...,n which is n(n+1)/2
    let mean_low = ((v.iter().fold(0, |acc, val| acc + val) as f64) / (v.len() as f64)).floor() as i64;
    let mean_high = ((v.iter().fold(0, |acc, val| acc + val) as f64) / (v.len() as f64)).ceil() as i64;
    let result2_low  = v.iter().map(|val| (val-mean_low).abs()).fold(0, |acc, n| acc + (n*(n+1))/2);
    let result2_high = v.iter().map(|val| (val-mean_high).abs()).fold(0, |acc, n| acc + (n*(n+1))/2);
    let result2 = min(result2_low, result2_high);
    //let result2b = v.iter().map(|val| (val-mean3).abs()).fold(0, |acc, n| acc + (n*(n+1))/2);
    // result2: 92676748 -> too high !? with floor (result2a), we get 92676746 (but this gives the *wrong* result for the example input)
    println!("07 - the treachery of whales: {} {}", result1, result2);
}