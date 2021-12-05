use regex::Regex;
use std::cmp::{max, min};

#[derive(Debug, PartialEq, Eq)]
enum Orientation { HORIZONTAL, VERTICAL, OTHER }

#[derive(Debug, PartialEq, Eq)]
pub struct Line {
    index: usize,
    x1: usize,
    y1: usize,
    x2: usize,
    y2: usize,
    orientation: Orientation,
}

impl Line {
    pub fn new(index: usize, x1: usize, y1: usize, x2: usize, y2: usize) -> Line {
        let orientation = 
                 if x1 == x2 { Orientation::VERTICAL } 
                 else if y1 == y2 { Orientation::HORIZONTAL }
                 else { Orientation::OTHER };
        /*let orientation = match x1 == x2 {
            false => Orientation::HORIZONTAL,
            true  => Orientation::VERTICAL
        };
        */
        return Line { index: index, x1: x1, y1: y1, x2: x2, y2: y2, orientation: orientation };
    }
}

fn parse_line(index: usize, line: &str) -> Line {
    let re = Regex::new(r"(\d+),(\d+) -> (\d+),(\d+)").unwrap();
    let md = re.captures(&line).unwrap();
    let x1: usize = md[1].parse().unwrap();
    let y1: usize = md[2].parse().unwrap();
    let x2: usize = md[3].parse().unwrap();
    let y2: usize = md[4].parse().unwrap();
    return Line::new(index, x1, y1, x2, y2);
}

fn add_line(grid: &mut Vec<Vec<usize>>, line: &Line) {
    match line.orientation {
        Orientation::HORIZONTAL => {
            // draw horizontal line
            let (start_x, end_x) = (min(line.x1, line.x2), max(line.x1, line.x2));
            for x in start_x .. end_x+1 {
                grid[line.y1][x] += 1;
            }
        },
        Orientation::VERTICAL => {
            // draw vertical line
            let (start_y, end_y) = (min(line.y1, line.y2), max(line.y1, line.y2));
            for y in start_y .. end_y+1 {
                grid[y][line.x1] += 1;
            }
        },
        Orientation::OTHER => {
            // do nothing
        }
    }
}

fn print_grid(grid: &Vec<Vec<usize>>) {
    for y in 0 .. grid.len() {
        let mut s = String::new();
        for x in 0 .. grid[y].len() {
            let char = match grid[y][x] {
                0 => '.',
                1..=9 => char::from_digit(grid[y][x] as u32, 10).unwrap(),
                _ => '*'
            };
            s.push(char);
        }
        println!("{}", s);
    }
}

pub fn max3(i: usize, j: usize, k: usize) -> usize {
    return max(max(i, j), k);
}

pub fn solve() {
    //let filename = "a05_hydrothermal_venture/example_input.txt";
    let filename = "a05_hydrothermal_venture/input.txt";
    let v = crate::helpers::read_string_list((&filename).to_string());
    let lines: Vec<Line> = v.iter().enumerate().map(|(idx, s)| parse_line(idx+1, s)).collect();
    // initialize grid with the correct dimensions
    let (max_x, max_y) = lines.iter().fold( (0,0), |acc, line| (max3(acc.0, line.x1, line.x2), max3(acc.1, line.y1, line.y2)));
    let mut grid: Vec<Vec<usize>> = (0 .. max_y+1).map(|row| vec![0; max_x + 1]).collect();
    // println!("dimensions: max_x = {}, max_y = {}", max_x, max_y);
    // "paint" the lines on the grid
    for line in lines {
        // println!("line: \n{:?}", line);
        add_line(&mut grid, &line);
        // print_grid(&grid);
    }
    // print_grid(&grid);
    let result1 = grid.iter().fold(0, |acc, row| acc + row.iter().fold(0, |acc2, cell| if *cell > 1 { acc2 + 1} else { acc2 }));
    let result2 = -1;
    println!("05 - hydrothermal venture: {} {}", result1, result2);
}

#[cfg(test)]
mod tests {
    use crate::a05_hydrothermal_venture::*;

    #[test]
    fn parse_line_example_1() {
        assert_eq!(Line { index: 0, x1: 0, y1: 9, x2: 5, y2: 9, orientation: Orientation::HORIZONTAL }, 
        parse_line(0, "0,9 -> 5,9"));
    }
    
}