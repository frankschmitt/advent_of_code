pub fn solve() {
    // let filename = "a07_the_treachery_of_whales/example_input.txt";
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
    let mean = ((v.iter().fold(0, |acc, val| acc + val) as f64) / (v.len() as f64)).round() as i64;
    let result2 = v.iter().map(|val| (val-mean).abs()).fold(0, |acc, n| acc + (n*(n+1))/2);
    // result2: 92676748 -> too high !?
    println!("07 - the treachery of whales: {} {}", result1, result2);
}