use std::io::{BufRead, BufReader};
use std::fs::File;

pub struct Grid {
    pub width: usize,
    pub height: usize,
    pub cells: Vec<Vec<char>>
}

pub struct GridIter<'a> {
    grid: &'a Grid,
    cur_row: usize,
    cur_col: usize
}

impl<'a> Iterator for GridIter<'a> {
    type Item = char;

    fn next(&mut self) -> Option<char> {
        // past last row? return none
        if self.cur_row >= self.grid.height() {
            return None;
        }
        // already at last element? return none
        if self.cur_row == self.grid.height()-1 && self.cur_col == self.grid.width()-1 {
            return None;
        }
        // last col? go to next row, col 0
        if self.cur_col == self.grid.width()-1 {
            self.cur_row += 1;
            self.cur_col = 0;
        }
        else {
            self.cur_col += 1;
        }
        return Some(self.grid.cell(self.cur_row, self.cur_col));
    }
}


// get the cell for the given row and column (wrapping around)
impl Grid {
    pub fn width(&self) -> usize {
        return self.width;
    }

    pub fn height(&self) -> usize {
        return self.height;
    }

    // get cell at given coordinates
    // wraps around
    pub fn cell(&self, row: usize, col: usize) -> char {
      let myrow = row % self.height;
      let mycol = col % self.width;
      // println!("getting cell at {}/{} -> {}/{}", row, col, myrow, mycol);
      let line = &self.cells[myrow];
      let ch= line[mycol];
      return ch;
    }

    // iterate over cells
    // this one doesn't wrap around (otherwise, we'd get an infinite iterator)
    pub fn cell_iter(&self) -> GridIter {
        GridIter {
            grid: self,
            cur_row: 0,
            cur_col: 0
        }
    }

    pub fn print(&self) -> () {
        for row in 0 .. self.height {
            let mut line: String = "".to_string();
            for col in 0 .. self.width  {
                line += &self.cell(row, col).to_string();
            }
            println!("{}", line);
        }
    }
}

pub fn read_char_grid(filename: String) -> Grid {
   let lines = read_string_list(filename);
   let mut cells = Vec::new();
   for line in lines {
      let chars: Vec<char> = line.chars().collect();
      cells.push(chars);
   }
   let grid = Grid { height: cells.len(),
                     width: cells[0].len(),
                     cells: cells };
   return grid;
}

pub fn read_int_list(filename: String) -> Vec<i64> {
    let reader = BufReader::new(File::open(filename).expect("Cannot open file"));
    let mut result = Vec::new();
    let mut val: i64;
    for line in reader.lines() {
        val = line.unwrap().parse().unwrap();
        result.push(val);
    }

    return result;
}

pub fn read_string_list(filename: String) -> Vec<String> {
    let reader = BufReader::new(File::open(filename).expect("Cannot open file"));
    let mut result = Vec::new();
    let mut val: String;
    for line in reader.lines() {
        val = line.unwrap();
        result.push(val);
    }

    return result;
}

pub fn read_string(filename: String) -> String {
    let reader = BufReader::new(File::open(filename).expect("Cannot open file"));
    let mut result = String::new();
    let mut val: String;
    for line in reader.lines() {
        val = line.unwrap();
        result += &(val + "\n");
    }

    return result;
}


pub fn result_to_string<T: std::fmt::Debug, E: std::fmt::Debug>(v:&Result<T,E>) -> String {
    return match v {
      Err(e) => format!("Err: {:#?}", e),
      Ok(t) => format!("Ok: {:#?}", t)
    };
}

// cute doesn't provide a version for three iterators 
/* macro_rules! c3 {
   ($exp:expr, for $i:ident in $iter:expr, for $i2:ident in $iter2:expr, for $i3:ident in $iter3:expr, if $cond:expr) => (
   {
   let mut r = vec![];
   for $i3 in $iter3 {
   for $i2 in $iter2 {
   for $i in $iter {
   if $cond{
   r.push($exp);
   }
   }
   }
   }
   r
   }
   );
   }
   */





