use regex::Regex;
use std::collections::VecDeque;
use itertools::Itertools;
use crate::helpers::Grid;

#[derive(Debug, Eq, PartialEq, Ord, PartialOrd)]
pub struct Point2D {
    x: usize,
    y: usize
}

#[derive(Debug, Eq, PartialEq)]
enum Fold {
  Horizontal { y: usize},
  Vertical { x: usize }
}

#[derive(Debug, Eq, PartialEq)]
pub struct Origami {
    points: Vec<Point2D>,
    folds: VecDeque<Fold>
}

impl Origami {
    pub fn step(&mut self) {
      let f = self.folds.pop_front().unwrap();
      let mut new_points: Vec<Point2D> = self.points.iter().map(|p| 
        match f {
            Fold::Horizontal { y } => Point2D { x: p.x, y: if p.y > y { p.y - 2*(p.y - y) } else { p.y }},
            Fold::Vertical   { x } => Point2D { x: if p.x > x { p.x - 2*(p.x - x) } else { p.x }, y: p.y},               
        }
      ).collect();
      new_points.sort();
      new_points.dedup();
      self.points = new_points;
    }

    pub fn run(&mut self) {
        loop {
            if self.folds.is_empty() { return };
            self.step();
        }
    }

    pub fn print(&self) {
      let num_cols = self.points.iter().map(|p| p.x).max().unwrap();
      let num_rows = self.points.iter().map(|p| p.y).max().unwrap();
      let mut grid = Grid::new(num_rows + 1, num_cols + 1, '.');
      for p in self.points.iter() {
          grid.set_cell(p.y, p.x, '#');
      }
      println!("grid: \n");
      grid.print();
    }
}

pub fn parse_origami(input: &Vec<String>) -> Origami {
    let mut points = vec![];
    let mut folds = VecDeque::new();
    let re_point = Regex::new(r"^(\d+),(\d+)$").unwrap();
    let re_fold = Regex::new(r"^fold along (x|y)=(\d+)$").unwrap();
    for line in input {
      if let Some(mdp) = re_point.captures(&line) {
        let x: usize = mdp[1].parse().unwrap();
        let y: usize = mdp[2].parse().unwrap();
        points.push(Point2D { x: x, y: y });
      }
      else if let Some(mdf) = re_fold.captures(&line) {
        let val: usize = mdf[2].parse().unwrap();
        let fold = match &mdf[1] {
            "x" => Fold::Vertical { x: val },
            _ => Fold::Horizontal { y: val }
        };
        folds.push_back(fold);
      }
    }
    return Origami { points: points, folds: folds };
}

pub fn solve() {
    //let filename = "a13_transparent_origami/example_input.txt";
    let filename = "a13_transparent_origami/input.txt";
    let v = crate::helpers::read_string_list((&filename).to_string());   
    let mut origami = parse_origami(&v);
    origami.step();
    let result1 = origami.points.len();
    origami.run();
    origami.print();
    let result2 = "AHPRPAUZ";
    println!("13 - transparent origami: {} {}", result1, result2);
}