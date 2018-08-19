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

    // get the cell for the given row and column; if indices are out of bounds:
    //    returns empty
    fn cell(&self, row: i32, col: i32) -> CellType {
      println!("getting cell at {} {}", row, col);
      if row < 0 || row >= self.num_rows || col < 0 || col >= self.num_cols {
        println!("  out of bounds!");
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

    // get the grid row for the given index
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

    fn find_next_horizontal_direction(&self, row: i32, col: i32) -> Direction {
      // try western neighbour; if it's empty, we need to go East, otherwise West
      match self.cell(row, col-1) {
         CellType::Empty => Direction::East,
         _ => Direction::West 
      }
    }

    fn find_next_vertical_direction(&self, row: i32, col: i32) -> Direction {
      // try southern neighbour; if it's empty, we need to go North, otherwise South 
      match self.cell(row+1, col) {
         CellType::Empty => Direction::North,
         _ => Direction::South 
      }
    }

    // find the new direction for a corner cell
    fn find_next_direction(&self, direction: Direction, row: i32, col: i32) -> Direction {
      let result = match direction {
        // we were walking vertically -> check horizontal neighbours
        Direction::South => self.find_next_horizontal_direction(row, col),
        Direction::North => self.find_next_horizontal_direction(row, col),
        // we were walking horizontally -> check vertical neighbours
        Direction::West => self.find_next_vertical_direction(row, col), 
        Direction::East => self.find_next_vertical_direction(row, col),
      };
      return result;
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
     let mut row : i32 = 0;
     let mut col : i32 = grid.cells[row as usize].iter().position(|&r| r == '|').unwrap() as i32;
     // println!("{}", match(start) { Some(x) => x, None => &'?'});
     println!("found start at: {}", col); 
     let mut done = false;
     let mut direction = Direction::South;
     let mut result = String::new();
     while !done  {
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
       let cell = grid.cell(row, col);
       match cell {
         CellType::Empty  => { done = true; }
         CellType::Letter { letter: c} => { result.push(c); }
         // Vertical and Horizontal bars are ignored - they never indicate a turn
         CellType::Vertical => {}
         CellType::Horizontal => {}
         CellType::Corner => {
           direction = grid.find_next_direction(direction, row, col);
         }
       }
     }
     return result;
  }

  #[test]
  fn it_creates_grid_from_input() {
    let res = read_grid_from_file("sample_input.txt");
    match res {
      Ok(grid) => { 
                    assert_eq!(6, grid.num_rows);
                    assert_eq!(16, grid.num_cols);
                    assert_eq!(CellType::Vertical, grid.cell(0, 5));
                    assert_eq!(CellType::Vertical, grid.cell(1, 5));
                    assert_eq!(CellType::Letter { letter: 'A' }, grid.cell(2, 5));
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

  #[test]
  fn it_solves_part_I() {
    let input = read_grid_from_file("input.txt");
    match input {
      Ok(grid) => {
                    let res = walk_grid(grid);
                    assert_eq!("SXPZDFJNRL", res);
                  },
      Err(_) => assert!(false),
    }
  }
}

fn main() {
  println!("Hello, world!");
}
