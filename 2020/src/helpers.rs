use std::io::{BufRead, BufReader};
use std::fs::File;

pub struct Grid {
    width: usize,
    height: usize,
    cells: Vec<Vec<char>>
}


// get the cell for the given row and column (wrapping around)
impl Grid {
    pub fn width(&self) -> usize {
        return self.width;
    }

    pub fn height(&self) -> usize {
        return self.height;
    }

    pub fn cell(&self, row: usize, col: usize) -> char {
      let myrow = row % self.height;
      let mycol = col % self.width;
      println!("getting cell at {}/{} -> {}/{}", row, col, myrow, mycol);
      let line = &self.cells[myrow];
      let ch= line[mycol];
      return ch;
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





