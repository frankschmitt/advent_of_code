use std::io::{BufRead, BufReader};
use std::fs::File;
use std::str;

pub struct Grid {
    pub width: usize,
    pub height: usize,
    pub cells: Vec<Vec<char>>
}

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


pub struct UIntGrid {
    pub width: usize,
    pub height: usize,
    pub cells: Vec<Vec<usize>>
}

impl UIntGrid {
    pub fn width(&self) -> usize {
        return self.width;
    }

    pub fn height(&self) -> usize {
        return self.height;
    }

    // get cell at given coordinates
    // wraps around
    pub fn cell(&self, row: usize, col: usize) -> usize {
      let myrow = row % self.height;
      let mycol = col % self.width;
      // println!("getting cell at {}/{} -> {}/{}", row, col, myrow, mycol);
      let line = &self.cells[myrow];
      let ch= line[mycol];
      return ch;
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

pub fn read_uint_grid(filename: String) -> UIntGrid {
   let lines = read_string_list(filename);
   let mut cells: Vec<Vec<usize>> = Vec::new();
   for line in lines {
      let chars: Vec<char> = line.chars().collect();
      let subs = line.as_bytes().chunks(1).map(|buf| unsafe { str::from_utf8_unchecked(buf) }).map(|val| val.parse::<usize>().unwrap()).collect();
      cells.push(subs);
   }
   let grid = UIntGrid { height: cells.len(),
                         width: cells[0].len(),
                         cells: cells };
   return grid;
}

// reads an int list (one int per line)
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

// reads an int list separated by commas
pub fn read_int_comma_separated_list(filename: String) -> Vec<i64> {
    let reader = BufReader::new(File::open(filename).expect("Cannot open file"));
    let mut result = Vec::new();
     for line in reader.lines() {
        let mut parts: Vec<i64> = line.unwrap().split(',').map(|val| val.parse::<i64>().unwrap()).collect();
        result.append(&mut parts);
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

