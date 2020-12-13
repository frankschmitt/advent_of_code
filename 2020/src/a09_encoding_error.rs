pub fn solve() {
    let filename = "a09_encoding_error/input.txt";
    let values = crate::helpers::read_int_list((&filename).to_string());
    // part 1: iterate over slices of length 25, and for all pairs within that slice, check
    // whether the sum equals the current value
    let mut idx = 25; // we start at index 25
    let result1 = loop {
        let found = c![x*y, for x in &values[idx-25 .. idx], for y in &values[idx-24 .. idx], 
        if x+y  == values[idx]].len() > 0;
        if found == false {
            break &values[idx];
        }
        idx += 1;
    };
    // part 2: find the sub-range whose sum equals result1, 
    //   and keep track of our sub-range by using two indices
    let mut i = 0;
    let mut j = 0;
    let mut sum = values[i];
    let _range2 = loop {
        if sum == *result1 {
            break (i,j);
        }
        else if sum > *result1 {
            sum -= values[i];
            i += 1;
        }
        else {
            j += 1;
            sum += values[j];
        }
    }; 
    // now, find the min and max in this range
    let min = values[i .. j+1].iter().min().unwrap();
    let max = values[i .. j+1].iter().max().unwrap();
    let result2 = min + max;
    println!("09 - encoding error: {} {}", result1, result2);
}

