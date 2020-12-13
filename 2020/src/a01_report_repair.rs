// detect all pairs (part 1) / triples (part 2) in the input whose sum equals 2020, and compute
// their product
pub fn solve() {
    let filename = "a01_report_repair/input.txt";
    let values = crate::helpers::read_int_list((&filename).to_string());
    // println!("got values: {}", values.len());
    let result1 = c![x*y, for x in &values, for y in &values, if x+y  == 2020 && x > y][0];
    // cute doesn't provide 3-iterator versions - therefore, use simple loops here
    let mut result2:i64 = -1;
    for x in &values {
        for y in &values {
            for z in &values {
                if x+y+z == 2020 {
                    result2 = x*y*z;
                }
            }
        }
    }

    println!("01 - report repair: {} {}", result1, result2);
}
