// we use the "cute" crate for Python-like list comprehensions, but due to Rust limitations, it has to be included at the crate root

pub fn solve() {
    let filename = "a01_sonar_sweep/input.txt";
    //let filename = "a01_sonar_sweep/example_input.txt";
    let a = crate::helpers::read_int_list((&filename).to_string());
    // part 1: count pairs of indices (i, i+1) where an increase takes place, i.e. a[i] < a[i+1]
    let increases1 = c![(a[i],a[i+1]), for i in 0 .. a.len()-1, if a[i] < a[i+1]];
    let result1 = increases1.len();
    // part 2: count pairs of indices where the three-measurement sum increases, i.e.
    let increases2 = c![(a[i]+a[i+1]+a[i+2],a[i+1]+a[i+2]+a[i+3]), for i in 0 .. a.len()-3, if a[i] + a[i+1] + a[i+2] < a[i+1] + a[i+2] + a[i+3] ];
    let result2 = increases2.len();
    println!("01 - sonar sweep: {} {}", result1, result2);
}
