use crate::helpers::UIntGrid;
use std::fmt;

#[derive(Debug)]
struct Octopus {
    grid: UIntGrid,
    num_flashes: usize,
    sync_flash_step_num: usize
}

impl Octopus {

    pub fn flash_neighbours(row: usize, col: usize, grid: &mut UIntGrid) {
      // W neighbour
      if col > 0 {
        grid.set_cell(row, col-1, grid.cell(row, col-1) + 1);    
      }
      // NW neighbour
      if row > 0 && col > 0 {
        grid.set_cell(row-1, col-1, grid.cell(row-1, col-1) + 1);
      }
      // N neighbour
      if row > 0 {
        grid.set_cell(row-1, col, grid.cell(row-1, col) + 1);
      }
      // NE neighbour
      if row > 0 && col < grid.width-1 {
        grid.set_cell(row-1, col+1, grid.cell(row-1, col+1) + 1);
      }    
      // E neighbour
      if col < grid.width-1 {
        grid.set_cell(row, col+1, grid.cell(row, col+1) + 1);
      }    
      // SE neighbour
      if row < grid.height-1 && col < grid.width-1 {
        grid.set_cell(row+1, col+1, grid.cell(row+1, col+1) + 1);
      }    
      // S neighbour
      if row < grid.height-1 {
        grid.set_cell(row+1, col, grid.cell(row+1, col) + 1);
      }
      // SW neighbour
      if row < grid.height-1 && col > 0 {
        grid.set_cell(row+1, col-1, grid.cell(row+1, col-1) + 1);
      }
    }

    // run a single step
    pub fn step(&mut self, step_num: usize) {
      let mut new_grid = UIntGrid { height: self.grid.height, width: self.grid.width, cells: self.grid.cells.clone() };
      // helper grid to remember where we have flashed
      let mut flashes = UIntGrid::new(self.grid.height, self.grid.width, 0);
      // step 1: increase all by 1
      for row in 0 .. new_grid.height {
          for col in 0 .. new_grid.width {
              let new_val = new_grid.cell(row, col) + 1;
              new_grid.set_cell(row, col, new_val);
          }
      }
      // step 2: flash em, and remember where we have flashed
      let mut num_flashes = 0;
      loop {
        let mut flashed = false;
        for row in 0 .. self.grid.height {
            for col in 0 .. self.grid.width {
                // flashing octopus? flash the neighbours
                if (new_grid.cell(row, col) > 9 && flashes.cell(row, col) == 0) {
                    Octopus::flash_neighbours(row, col, &mut new_grid);
                    flashed = true;
                    flashes.set_cell(row, col, 1);
                    num_flashes += 1;
                }
            }
        }
        if flashed == false { break; }
      }
      // step 3: set flashed ones to 0
      for row in 0 .. self.grid.height {
        for col in 0 .. self.grid.width {
            if (new_grid.cell(row, col) > 9) {
                new_grid.set_cell(row, col, 0);
            }
        }
      }
      self.grid = new_grid;
      self.num_flashes += num_flashes;
      if num_flashes == self.grid.height * self.grid.width && self.sync_flash_step_num == 0 {
        self.sync_flash_step_num = step_num;
      }
    }

    // run step indices start_n till end_n (including) or (if stop_at_sync is set) until we sync flash for the first time
    pub fn run(&mut self, start_n: usize, end_n: usize, stop_at_sync: bool) {
      for i in start_n ..=end_n {
        self.step(i);
        if self.sync_flash_step_num > 0 && stop_at_sync {
          break;
        }
      }
    }
}

impl fmt::Display for Octopus {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let _ = write!(f, "{}", self.grid);
    return Ok(());
  }
}

pub fn solve() {
    //let filename = "a11_dumbo_octopus/example_input.txt";
    let filename = "a11_dumbo_octopus/input.txt";
    let grid = crate::helpers::read_uint_grid((&filename).to_string());
    let mut octopus = Octopus { grid: grid, num_flashes: 0, sync_flash_step_num: 0 };
    octopus.run(1, 100, false);
    let result1 = octopus.num_flashes;
    // we assume 1e9 steps will be sufficient :-)
    octopus.run(101, 1_000_000_000, true);
    let result2 = octopus.sync_flash_step_num;
    println!("11 - dumbo octopus: {} {}", result1, result2);
}
