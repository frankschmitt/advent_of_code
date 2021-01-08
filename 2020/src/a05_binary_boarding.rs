pub fn solve() {
    let filename = "a05_binary_boarding/input.txt";
    let input = crate::helpers::read_string_list(filename.to_string());
    let mut passes: Vec<u64> = input.iter().map(|line| seat_id(line)).collect::<Vec<_>>();
    passes.sort();
    let result1 = passes.iter().max().unwrap();
    let mut result2: u64 = 0;
    let mut previous: u64 = 0;  
    passes.iter().for_each(|p| 
        {
          // difference equals 2? then we have a gap (aka our seat)
          if *p == previous+2 {
            result2 = p-1;
          }
          previous = *p;
        }
    );
    println!("05 - binary boarding: {} {}", result1, result2);
}

/// compute the seat ID for the given 
pub fn seat_id(v: &str) -> u64 {
    let (row, col) = v.split_at(7);
    let row_idx = u64::from_str_radix(&row.replace("F", "0").replace("B", "1"), 2).unwrap();
    let col_idx = u64::from_str_radix(&col.replace("L", "0").replace("R", "1"), 2).unwrap();
    return row_idx*8 + col_idx;
}

#[cfg(test)]
mod tests {

    use crate::a05_binary_boarding::seat_id;

    #[test]
    fn test_should_return_357_for_example_boarding_pass() {
      assert_eq!(357, seat_id("FBFBBFFRLR"));
    }
}
