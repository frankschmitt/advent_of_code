use ndarray::*;
use std::fmt;

#[derive(Debug, Eq, PartialEq)]
pub struct Board {
    elems: Array2<i64>,
    winning_score: i64
}

#[derive(Debug, Eq, PartialEq)]
enum Update { NoMatch, Match, MatchAndWin }

impl std::fmt::Display for Board {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
      let _ = write!(f, "{:?}", self.elems);
      return write!(f, "winning_score: {}\n", self.winning_score);
    }
}

// a Bingo Board. We represent it as a simple 2D-Array of signed ints
//  if a number is marked, we subtract 1000 from it, making it negative
//  to compute the score, we then have to add the 1000 again
impl Board {
    // calculate board score: sum of unmarked fields times the winning number
    fn calc_score(&self, num: i64) -> i64 {
        return self.elems.iter().filter(|v| **v > 0).fold(0, |acc, x| acc + x.abs()) * num;
    }

    // mark the given number
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
}

#[derive(Debug, Eq, PartialEq)]
pub struct Bingo {
    numbers: Vec<i64>,
    boards: Vec<Board>, 
    winning_score: i64, 
    losing_score: i64
}

impl std::fmt::Display for Bingo {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
      for b in self.boards.iter() {
        let _ = write!(f, "{}", b);
      }
      return write!(f, "\n");
    }
}

impl Bingo {
    // run the bingo game - draw numbers until a winner is determined
    pub fn run(&mut self) {
        for n in self.numbers.iter() {
            let num_boards = self.boards.len();
            let mut boards_to_remove = vec![];
            for (idx, b) in self.boards.iter_mut().enumerate() {
                if b.mark(*n) == Update::MatchAndWin {
                    // println!("----- WINNNER! score: {} --------\n", b.winning_score); 
                    // last board? set losing score + exit
                    if num_boards == 1 {
                        self.losing_score = b.winning_score;
                        return;
                    }
                    // first board? set winning score 
                    if self.winning_score == 0 { 
                        self.winning_score = b.winning_score; 
                    }
                    // mark board for removal
                    //  we add higher indices at the front so removing them in-order later on doesn't cause problems due to shifted indices
                    boards_to_remove.insert(0, idx); 
                }
            }
            // remove winning boards
            for idx in boards_to_remove {
                self.boards.remove(idx);
            }
        }
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
    return Bingo { numbers: numbers, boards: boards, winning_score: 0, losing_score: 0};
}

pub fn solve() {
    let filename = "a04_giant_squid/input.txt";
    // let filename = "a04_giant_squid/example_input.txt";
    let v = crate::helpers::read_string_list((&filename).to_string());   
    let mut bingo = parse_bingo(&v);
    bingo.run();
    let result1 = bingo.winning_score;
    let result2 = bingo.losing_score;
    println!("04 - giant squid: {} {}", result1, result2);
}