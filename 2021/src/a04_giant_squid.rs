use ndarray::*;
use std::fmt;

#[derive(Debug, Eq, PartialEq)]
pub struct Cell {
    num: u64,
    marked: bool
}

#[derive(Debug, Eq, PartialEq)]
pub struct Board {
    //elems: Array2<Cell>,
    elems: Array2<i64>,
    winning_score: i64
}

#[derive(Debug, Eq, PartialEq)]
enum Update { NoMatch, Match, MatchAndWin }

impl std::fmt::Display for Board {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
      write!(f, "{:?}", self.elems);
      return write!(f, "\nwinner? : {}, winning_score: {}\n", self.is_winner(), self.winning_score);
    }
}

// a Bingo Board. We represent it as a simple 2D-Array of signed ints
//  if a number is marked, we subtract 1000 from it, making it negative
//  to compute the score, we then have to add the 1000 again
impl Board {
    /*fn calc_col_score(&self, col_idx: usize) -> i64 {
        return self.elems.column(col_idx).sum().abs();
    }

    fn calc_row_score(&self, row_idx: usize) -> i64 {
        return self.elems.row(row_idx).sum().abs();
    }*/

    fn calc_score(&self, num: i64) -> i64 {
        return self.elems.iter().filter(|v| **v > 0).fold(0, |acc, x| acc + x.abs()) * num;
    }

    // mark the given number; returns true if 
    pub fn mark(&mut self, num: i64) -> Update {   
        let mut result = Update::NoMatch;
        for r in 0 .. self.elems.nrows() {
            for c in 0 .. self.elems.ncols() {
              if self.elems[[r, c]] == num {
                  self.elems[[r, c]] -= 1000; // make value negative to indicate it is marked
                  result = Update::Match;
                  if self.elems.column(c).iter().all(|elem| *elem < 0) { result = Update::MatchAndWin; self.winning_score = self.calc_score(num); }
                  if self.elems.row(r).iter().all(|elem| *elem < 0) { result = Update::MatchAndWin; self.winning_score = self.calc_score(num); }
              }        
            }
        }
        return result;     
    } 

    // check whether we have won 
    pub fn is_winner(&self) -> bool {
        for c in self.elems.columns() {
            if c.iter().all(|elem| *elem < 0) { return true; }
        }
        for r in self.elems.rows() {
            if r.iter().all(|elem| *elem < 0 ) { return true; }
        }
        return false;
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct Bingo {
    numbers: Vec<i64>,
    boards: Vec<Board>
}

impl std::fmt::Display for Bingo {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
      for b in self.boards.iter() {
        write!(f, "{}", b);
      }
      return write!(f, "\n");
    }
}

impl Bingo {
    // run the bingo game - draw numbers until a winner is determined
    pub fn run(&mut self) -> i64 {
        for n in self.numbers.iter() {
            for b in self.boards.iter_mut() {
                b.mark(*n);
                if b.is_winner() { println!("----- WINNNER! score: {} --------\n", b.winning_score); return b.winning_score; }
            }
        }
        return 0;
    }
}

pub fn parse_boards(input: &[String]) -> Vec<Board> {
    let mut result = vec![];
    // each boards is 5x5 followed by an empty line - let's chunk it!
    for chk in input.chunks(6) {
        let mut number_vec = vec![];
        for i in 0 .. 5 {
            // println!("parsing chunk: {}", chk[i]);
            let mut parts: Vec<i64> = chk[i].split_whitespace().map(|val| val.parse::<i64>().unwrap()).collect();
            number_vec.append(&mut parts);
        }
        let elems = Array::from_shape_vec((5, 5), number_vec).unwrap();
        result.push(Board { elems: elems, winning_score: 0 });
    }
    return result;
}

pub fn parse_bingo(input: &Vec<String>) -> Bingo {
    let line = input.first().unwrap();
    let numbers: Vec<i64> = line.split(',').map(|val| val.parse::<i64>().unwrap()).collect();
    let boards = parse_boards(&input[2..]);
    return Bingo { numbers: numbers, boards: boards};
}

pub fn solve() {
     let filename = "a04_giant_squid/input.txt";
    //let filename = "a04_giant_squid/example_input.txt";
    let v = crate::helpers::read_string_list((&filename).to_string());   
    let mut bingo = parse_bingo(&v);
    
    bingo.run();

    //println!("bingo: {}", bingo);

    let result1 = -1;
    let result2 = -1;
    println!("04 - giant squid: {} {}", result1, result2);
}