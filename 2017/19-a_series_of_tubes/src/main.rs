#[cfg(test)]
mod tests {

  // use std::fs as fs;
  use std::fs::File;
  use std::path::Path;
  use std::io::{self, BufRead};
  use std::char;
    
  #[derive(Debug, PartialEq)]
  enum Direction { 
    North,
    East,
    South,
    West
  }

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
    cells: Vec<Vec<char>>
  }

  impl Grid {

    fn cell(&self, row: i32, col: i32) -> CellType {
      if row >= self.num_rows || col >= self.num_cols {
        return CellType::Empty;
      }
      let line = &self.cells[row as usize];
      let ch= &line[col as usize];
      match ch {
          ' ' => CellType::Empty,
          '|' => CellType::Vertical,
          '-' => CellType::Horizontal,
          '+' => CellType::Corner,
          c   => CellType::Letter { letter: *c }
       }
    }

    fn row(&self, row: i32) -> Vec<CellType> {
      if row >= self.num_rows {
        return Vec::new();
      }
      let line = &self.cells[row as usize];
      return line.iter().map(|&ch| match ch {
                              ' ' => CellType::Empty,
                              '|' => CellType::Vertical,
                              '-' => CellType::Horizontal,
                              '+' => CellType::Corner,
                              c   => CellType::Letter { letter: c }
                            }).collect::<Vec<_>>();
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
        Ok(s) => {
          let chars: Vec<char> = s.chars().collect();
          cells.push(chars);
        },
        Err(_) => println!("error!"), // ignore errors for now
      }
    }
    let grid = Grid { num_rows: cells.len() as i32, 
                      num_cols: cells[0].len() as i32,
                      cells: cells };
    return Ok(grid);
  }

  fn walk_grid(grid: Grid) -> String
  {
     let mut row = 0;
     let mut col = grid.cells[row].iter().position(|&r| r == '|').unwrap();
     // println!("{}", match(start) { Some(x) => x, None => &'?'});
     println!("{}", col); 
     let mut done = false;
     let mut direction = Direction::South;
     let mut result = String::new();
     while(!done) {
       // determine next cell
       match direction {
         Direction::South => {
           row += 1;
           } 
         Direction::East => {
           col += 1;
         }
         Direction::North => {
           row -= 1;
         }
         Direction::West => {
           col -= 1;
         }
       }
       done = true; 
     }
     return result;
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

  #[test]
  fn it_walks_the_sample_grid() {
    let input = read_grid_from_file("sample_input.txt");
    match input {
      Ok(grid) => {
                    let res = walk_grid(grid);
                    assert_eq!("ABCDEF", res);
                  },
      Err(_) => assert!(false),
    }
  }
}

fn main() {
  println!("Hello, world!");
}
