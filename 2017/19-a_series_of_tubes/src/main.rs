#[cfg(test)]
mod tests {

  // use std::fs as fs;
  use std::fs::File;
  use std::path::Path;
  use std::io::{self, BufRead};
    
  enum Direction {
    North,
    East,
    South,
    West
  }

  struct Grid {
    num_rows: i32,
    num_cols: i32
  }

  fn read_grid_from_file<P>(filename: P) -> Result<Grid, io::Error>  
    where P: AsRef<Path>
  {
    let file = try!(File::open(filename));
    let lines = io::BufReader::new(file).lines();
    let mut vec = Vec::new();
    for line in lines {
      match line {
        Ok(s) => vec.push(s),
        Err(_) => println!("error!"), // ignore errors for now
      }
    }
    // let input = fs::read_to_string(filename).expect("Unable to read file");
    
    let grid = Grid { num_rows: vec.len() as i32, num_cols: vec[0].len() as i32 };
    return Ok(grid);
  }

  #[test]
  fn it_creates_grid_from_input() {
    let res = read_grid_from_file("sample_input.txt");
    match res {
      Ok(grid) => { 
                    assert_eq!(7, grid.num_rows);
                    assert_eq!(16, grid.num_cols);
                  },
      Err(_) => assert!(false),
    }
  } 

}

fn main() {
  println!("Hello, world!");
}
