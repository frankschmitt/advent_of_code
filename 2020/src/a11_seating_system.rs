use crate::helpers::Grid;



fn step(grid: &Grid) -> Grid {
  let mut cells = Vec::new();
  for row in 0 .. grid.height() {
      let mut line: Vec<char> = vec![];
      for col in 0 .. grid.width() {
      }
      cells.push(line);
  }
  let result = Grid { cells: cells, .. (*grid) };
  return result;
}

/// count the number of occupied seats in a grid
/// a seat is occupied if the associated cell contains a '#' sign
fn get_occupied_seat_count(grid: &Grid) -> usize {
  return grid.cell_iter().filter (|cell| *cell == '#').count();
}

pub fn solve() {
    let result1 =-1;
    let result2 = -1;
    println!("11 - seating system: {} {}", result1, result2);
}

pub fn read_input(filename: &str) -> Grid {
    let grid = crate::helpers::read_char_grid(filename.to_string());
    return grid;
}

#[cfg(test)]
mod tests {
    use crate::a11_seating_system::*;

    fn read_test_input() -> Grid {
        let filename = "src/a11_seating_system/test_input.txt";
        return read_input(&filename);
    } 

    #[test]
    fn number_of_occupied_seats_for_test_input() {
        let grid = read_test_input();
        assert_eq!(0, get_occupied_seat_count(&grid));
    }

    #[test]
    fn number_of_occupied_seats_after_stabilizing_for_test_input() {
        let grid = read_test_input();
        assert_eq!(2, 1);
    }
}