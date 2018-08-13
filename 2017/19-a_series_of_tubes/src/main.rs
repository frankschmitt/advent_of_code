#[cfg(test)]
mod tests {

  // use std::fs as fs;
  use std::fs::File;
  use std::path::Path;
  use std::io::{self, BufRead};
  use std::char;
    
  #[derive(Debug, PartialEq)]
  enum CellType {
    Empty,
    Vertical,
    Horizontal,
    Corner,
    Letter { letter: char }
  }

  #[derive(Debug, Clone)]
  struct Grid {
    num_rows: i32,
    num_cols: i32,
    cells: Vec<String>
  }

  impl Grid {

    fn cell(&self, row: i32, col: i32) -> CellType {
      if row >= self.num_rows || col >= self.num_cols {
        return CellType::Empty;
      }
      let line = &self.cells[row as usize];
      let byte = &line.into_bytes()[col as usize];
      match char::from_u32(*byte as u32) { 
        Some(ch) => match ch {
          ' ' => CellType::Empty,
          '|' => CellType::Vertical,
          '-' => CellType::Horizontal,
          '+' => CellType::Corner,
          c   => CellType::Letter { letter: c }
       },
       None => CellType::Empty
      }
    }
  }

  fn read_grid_from_file<P>(filename: P) -> Result<Grid, io::Error>  
    where P: AsRef<Path>
  {
    let file = try!(File::open(filename));
    let lines = io::BufReader::new(file).lines();
    let mut cells = Vec::new();
    for line in lines {
      match line {
        Ok(s) => cells.push(s),
        Err(_) => println!("error!"), // ignore errors for now
      }
    }
    // let input = fs::read_to_string(filename).expect("Unable to read file");
    let grid = Grid { num_rows: cells.len() as i32, 
                      num_cols: cells[0].len() as i32,
                      cells: cells };
    return Ok(grid);
  }

  #[test]
  fn it_creates_grid_from_input() {
    let res = read_grid_from_file("sample_input.txt");
    match res {
      Ok(grid) => { 
                    assert_eq!(7, grid.num_rows);
                    assert_eq!(16, grid.num_cols);
                    assert_eq!(CellType::Vertical, grid.cell(0, 5));
                    assert_eq!(CellType::Vertical, grid.cell(1, 5));
                    assert_eq!(CellType::Letter { letter: 'A' }, grid.cell(2, 5));
                  },
      Err(_) => assert!(false),
    }
  } 

}

fn main() {
  println!("Hello, world!");
}
