use ndarray::prelude::*;

pub fn solve() {
    let filename = "a03_binary_diagnostic/input.txt";
    //let filename = "a03_binary_diagnostic/example_input.txt";
    let v = crate::helpers::read_string_list((&filename).to_string());
    // convert our vector of strings into a 2d-matrix of characters (each cell holds one character - either 0 or 1)
    let rows = v.len();
    let cols = v[0].len();
    let chars: Vec<char> = v.iter().map (|w| w.chars()).flatten().collect();
    let mat = Array::from_iter(chars.iter()).into_shape([rows, cols]).unwrap();
    //println!("mat = \n{}\n", mat);
    // for each column: count the number of zeroes and ones, and append the more frequent one to our result 
    let mut delta_s = String::new();
    let mut epsilon_s = String::new();
    for c in 0 .. cols {
      let col = mat.slice(s![.., c]);
      let current_counts = col.iter().fold((0,0), |(c0, c1), elem| if **elem == '0' { (c0+1, c1)} else {(c0, c1+1)});
      //println!("column {} -> \n{}\n", c, col);
      //println!("counts for column {} -> \n{:?}\n", c, current_counts);
      if current_counts.0 > current_counts.1 { 
          delta_s.push('0');
          epsilon_s.push('1'); 
        }
      else { 
          delta_s.push('1');
          epsilon_s.push('0');
        }
    }
    let delta = usize::from_str_radix(&delta_s, 2).unwrap();
    let epsilon = usize::from_str_radix(&epsilon_s, 2).unwrap();  
    let result1 = delta * epsilon;
    let result2 = -1;
    println!("03 - binary diagnostic: {} {}", result1, result2);
}
