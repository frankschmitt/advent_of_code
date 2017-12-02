#[cfg(test)]

mod tests {

  fn corruption_checksum(vals: Vec<i32>) -> i32 {
    let biggest = vals.iter().max();
    let smallest = vals.iter().min();
    match (biggest, smallest) {
      (Some(a), Some (b)) => a - b,
      _ => -1
    } 
  }


  #[test]
  fn checksum_should_return_max_minus_min_for_two_elem_matrix() {
    assert_eq!(corruption_checksum(vec![1,5]), 4);
    assert_eq!(corruption_checksum(vec![1,4]), 3);
  }
}


fn main() {
    println!("Hello, world!");
}
