fn print_candidates(mat: &Vec<Vec<char>>, indices: &Vec<usize>) {
  for idx in indices {
    println!("  {:?}", mat[*idx]);
  }
}

#[derive(Eq, PartialEq)]
enum RatingType { OXYGEN, CO2 }

fn get_rating(mat: &Vec<Vec<char>>, rows: usize, cols: usize, rating_type: RatingType) -> usize {
  let mut candidates: Vec<usize> = (0 .. rows).collect();
  let mut current_col_idx: usize = 0;
  loop {  
    // determine the target char for the current position and rating
    //   for Oxygen: take the most frequent one; if zeroes and ones are equally frequent: take '1'
    //   for CO2:    take the less frequent one; if zeroes and ones are equally frequent: take '0'
    //let col = mat.slice(s![candidates, current_col_idx]);
    let col: Vec<char> = mat.iter().map(|row| row[current_col_idx]).collect();
    let current_counts = col.iter().fold((0,0), |(c0, c1), elem| if *elem == '0' { (c0+1, c1)} else {(c0, c1+1)});
    let mut target_char: char;
    if rating_type == RatingType::OXYGEN {
        if current_counts.1 >= current_counts.0 { target_char = '1'; } 
        else { target_char = '0'; }
      }
    else if rating_type == RatingType::CO2 {
      if current_counts.1 >= current_counts.0 { target_char = '0'; } 
      else { target_char = '1'; }
    }
    // only keep candidates with the correct target char in the current position
    //candidates.retain (|&c| *mat[[c, current_col_idx]] == target_char);
    println!("idx = {}, candidates: {:?}", current_col_idx, candidates);
    print_candidates(&mat, &candidates);
    current_col_idx += 1;
    if candidates.len() == 1 { break; }
  }
  for c in candidates {
    return c;
  }
  return 1;
}

// Overall approach:
//   preparation
//     - convert input into a 2d-matrix of characters; each element is either 0 or 1
//   part 1:
//     - for each column in the matrix: 
//       - compute the number of zeroes and ones
//       - set the position of gamma and epsilon accordingly
//     - compute solution by multiplying gamma and epsilon
//   part 2:
//     
pub fn solve() {
    let filename = "a03_binary_diagnostic/input.txt";
    //let filename = "a03_binary_diagnostic/example_input.txt";
    let v = crate::helpers::read_string_list((&filename).to_string());
    // convert our vector of strings into a 2d-matrix of characters (each cell holds one character - either 0 or 1)
    let rows = v.len();
    let cols = v[0].len();
    //let chars: Vec<char> = v.iter().map (|w| w.chars()).flatten().collect();
    //let mat = Array::from_iter(chars.iter()).into_shape([rows, cols]).unwrap();
    // part 1: for each column: count the number of zeroes and ones, and append the more frequent one to our result 
    //let mat: Vec<char> = v.iter().map (|w| w.chars()).flatten().collect();
    let mat: Vec<Vec<char>> = v.iter().map (|row| row.chars().collect()).collect();
    let mut gamma_s = String::new();
    let mut epsilon_s = String::new();
    for c in 0 .. cols {
      //let col = mat.slice(s![.., c]);
      let col: Vec<char> = mat.iter().map(|row| row[c]).collect();
      let current_counts = col.iter().fold((0,0), |(c0, c1), elem| if *elem == '0' { (c0+1, c1)} else {(c0, c1+1)});
      if current_counts.0 > current_counts.1 { 
          gamma_s.push('0');
          epsilon_s.push('1'); 
        }
      else { 
          gamma_s.push('1');
          epsilon_s.push('0');
        }
    }
    let gamma_chars: Vec<char> = gamma_s.chars().collect();
    let epsilon_chars: Vec<char> = epsilon_s.chars().collect();
    let gamma = usize::from_str_radix(&gamma_s, 2).unwrap();
    let epsilon = usize::from_str_radix(&epsilon_s, 2).unwrap();  
    let result1 = gamma * epsilon;
    // part 2: successively filter the input lines by the current most (least) frequent bit until only one number remains
    // rather than removing whole strings, we just keep lists of the current candidates' indices
    /*let mut oxygen_candidates: Vec<usize> = (0 .. rows).collect();
    let mut idx_oxygen: usize = 0;
    println!("mat: \n{:?}\n", mat);
    println!("gamma {}, epsilon: {}", gamma_s, epsilon_s);
    loop
    { oxygen_candidates.retain (|&c| *mat[[c, idx_oxygen]] == gamma_chars[idx_oxygen]);
      println!("idx = {}, oxygen_candidates: {:?}", idx_oxygen, oxygen_candidates);
      print_candidates(&mat, &oxygen_candidates);
      idx_oxygen += 1;
      if oxygen_candidates.len() == 1 { break; }
    }
    let mut co2_candidates: HashSet<usize> = (0 .. rows).collect();
    let mut idx_co2: usize = 0;
    loop 
    { 
      co2_candidates.retain (|&c| *mat[[c, idx_co2]] == epsilon_chars[idx_co2]);
      idx_co2 += 1;
      if co2_candidates.len() == 1 { break; }
    }
    println!("oxygen: {:?}, co2: {:?}", oxygen_candidates, co2_candidates);
    */
    let result2 = -1;
    println!("03 - binary diagnostic: {} {}", result1, result2);
}
