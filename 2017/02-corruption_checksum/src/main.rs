#[cfg(test)]

// problem: corruption checksum
// language: Rust

mod tests {

  type Row = Vec<u32>;
  type Matrix = Vec<Row>;

  // compute the checksum for a single row 
  // returns the difference between max and min; 0 for an empty row
  fn row_checksum(vals: Row) -> u32 {
    let biggest = vals.iter().max();
    let smallest = vals.iter().min();
    match (biggest, smallest) {
      (Some(a), Some (b)) => a - b,
      _ => 0 
    } 
  }

  // compute the checksum for a matrix by computing the sum of the row checksums
  fn matrix_checksum(mat: Matrix) -> u32 {
    mat.iter().fold(0u32, |sum, val| sum + row_checksum(val.to_vec()))
  }

  // compute the division result for a single row
  // returns a/b for the two values in row for which "a is evenly divided by b" holds
  fn row_division(row: &Row) -> u32 { 
     // this is terribly inefficient: n**2 where n denotes the number of elements in row
     for x in row {
       for y in row {
         if ((x % y) == 0) && (x != y) {
            return x / y;
         } 
       }
     } 
     0 // dummy value - I'm too lazy to correctly handle wrong inputs
  }

  // compute the division result for a matrix by computing the sum of the row divisions
  fn matrix_division(mat: &Matrix) -> u32 {
    mat.iter().fold(0u32, |sum, val| sum + row_division(val))
  }

  // TESTS
  #[test]
  fn row_checksum_should_return_max_minus_min() {
    assert_eq!(row_checksum(vec![1,5]), 4);
    assert_eq!(row_checksum(vec![1,4]), 3);
  }

  #[test]
  fn matrix_checksum_should_return_sum_of_row_checksums_for_example() {
    let input = vec![
      vec![5, 1, 9, 5],
      vec![7, 5, 3],
      vec![2, 4, 6, 8]
    ];
    // checksum: (9-1) + (7-3) + (8-2) = 8 + 4 + 6 = 18
    assert_eq!(matrix_checksum(input), 18);
  }

  #[test]
  fn row_division_should_return_division_result() {
    assert_eq!(row_division(&vec![5,9,2,8]), 4);
    assert_eq!(row_division(&vec![5,9,4,8]), 2);
  }

  #[test]
  fn matrix_division_checksum_should_return_sum_of_division_results_for_example() {
    let input = vec![
      vec![5, 9, 2, 8],
      vec![9, 4, 7, 3],
      vec![3, 8, 6, 5]
     ];
    // (8 / 2) + (9 / 3) + (6 / 3) = 4 + 3 + 2 = 9
    assert_eq!(matrix_division(&input), 9);
  }

  fn challenge_input() -> Matrix {
   vec![
    vec![ 62,1649,1731,76,51,1295,349,719,52,1984,2015,2171,981,1809,181,1715],
    vec![ 161,99,1506,1658,84,78,533,242,1685,86,107,1548,670,960,1641,610],
    vec![ 95,2420,2404,2293,542,2107,2198,121,109,209,2759,1373,1446,905,1837,111],
    vec![ 552,186,751,527,696,164,114,530,558,307,252,200,481,142,205,479],
    vec![ 581,1344,994,1413,120,112,656,1315,1249,193,1411,1280,110,103,74,1007],
    vec![ 2536,5252,159,179,4701,1264,1400,2313,4237,161,142,4336,1061,3987,2268,4669],
    vec![ 3270,1026,381,185,293,3520,1705,1610,3302,628,3420,524,3172,244,295,39],
    vec![ 4142,1835,4137,3821,3730,2094,468,141,150,3982,147,4271,1741,2039,4410,179],
    vec![ 1796,83,2039,1252,84,1641,2165,1218,1936,335,1807,2268,66,102,1977,2445],
    vec![ 96,65,201,275,257,282,233,60,57,200,216,134,72,105,81,212],
    vec![ 3218,5576,5616,5253,178,3317,6147,5973,2424,274,4878,234,200,4781,5372,276],
    vec![ 4171,2436,134,3705,3831,3952,2603,115,660,125,610,152,4517,587,1554,619],
    vec![ 2970,128,2877,1565,1001,167,254,2672,59,473,2086,181,1305,162,1663,2918],
    vec![ 271,348,229,278,981,1785,2290,516,473,2037,737,2291,2521,1494,1121,244],
    vec![ 2208,2236,1451,621,1937,1952,865,61,1934,49,1510,50,1767,59,194,1344],
    vec![ 94,2312,2397,333,1192,106,2713,2351,2650,2663,703,157,89,510,1824,125]
  ]
  }

  #[test]
  fn solve_puzzle() {
  assert_eq!(matrix_checksum(challenge_input()), 44216);
  assert_eq!(matrix_division(&challenge_input()), 320);
 } 
}


fn main() {
    println!("Hello, world!");
}
