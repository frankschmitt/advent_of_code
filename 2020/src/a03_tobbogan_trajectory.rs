pub fn solve() {
    struct Walk {
        x: usize,
        y: usize,
        delta_x: usize,
        delta_y: usize,
        trees: usize
    }

    let filename = "a03_tobbogan_trajectory/input.txt";
    let grid = crate::helpers::read_char_grid(filename.to_string());
    let walks = vec![Walk { x:0, y:0, delta_x:1, delta_y : 1, trees : 0},
                     Walk { x:0, y:0, delta_x:3, delta_y : 1, trees : 0},
                     Walk { x:0, y:0, delta_x:5, delta_y : 1, trees : 0},
                     Walk { x:0, y:0, delta_x:7, delta_y : 1, trees : 0},
                     Walk { x:0, y:0, delta_x:1, delta_y : 2, trees : 0}
                    ];
    let mut result1 = 0;
    let mut result2 = 1;
    for mut w in walks {
        while w.y < grid.height() {
            let c = grid.cell(w.y, w.x); // y: row, x: column !
            if c == '#' {
                w.trees += 1;
            }
            w.x += w.delta_x;
            w.y += w.delta_y;
        }
        if w.delta_x == 3 {
            result1 = w.trees;
        }
        result2 = result2 * w.trees;
    }
    println!("03 - tobbogan trajectory: {} {}", result1, result2);
}

#[cfg(test)]
mod tests {

    fn read_test_input_grid() -> crate::helpers::Grid {
      let filename = "src/a03_tobbogan_trajectory/test_input.txt";
      let grid = crate::helpers::read_char_grid(filename.to_string());
      return grid;
    }

    #[test]
    fn grid_for_test_input_should_have_correct_dimensions() {
      let grid =read_test_input_grid();
      assert_eq!(11, grid.height());
      assert_eq!(11, grid.width());

    }

    #[test]
    fn grid_for_real_input_should_have_correct_dimensions() {
      let filename = "src/a03_tobbogan_trajectory/input.txt";
      let grid = crate::helpers::read_char_grid(filename.to_string());
      assert_eq!(323, grid.height());
      assert_eq!(31, grid.width());

    }

    #[test]
    fn cell_for_test_input_within_bounds_should_return_content() {
      let grid =read_test_input_grid();
      // first row
      assert_eq!('.', grid.cell(0, 0));
      assert_eq!('#', grid.cell(0, 2));
      assert_eq!('.', grid.cell(0, 10));

      // last row
      assert_eq!('.', grid.cell(10, 0));
      assert_eq!('#', grid.cell(10, 1));
      assert_eq!('#', grid.cell(10, 10));
    }

    #[test]
    fn cell_for_test_input_out_of_bounds_should_wrap_around() {
      let grid =read_test_input_grid();
      // one after last row -> first row
      assert_eq!('.', grid.cell(11, 0));
      assert_eq!('#', grid.cell(11, 2));
      assert_eq!('.', grid.cell(12, 10));
      // one after last column -> first column
      assert_eq!('.', grid.cell(0, 11));
      assert_eq!('.', grid.cell(10,11));
    }

    #[test]
    fn cell_for_test_input_should_give_expected_result() {
      let grid =read_test_input_grid();
      // one after last row -> first row
      assert_eq!('.', grid.cell(0, 0));
      assert_eq!('.', grid.cell(1, 3));
      assert_eq!('#', grid.cell(2, 6));
      assert_eq!('.', grid.cell(3, 9));
      assert_eq!('#', grid.cell(4,12));
      assert_eq!('#', grid.cell(5,15));
      assert_eq!('.', grid.cell(6, 18));
      assert_eq!('#', grid.cell(7, 21));
      assert_eq!('#', grid.cell(8, 24));
      assert_eq!('#', grid.cell(9, 27));
      assert_eq!('#', grid.cell(10,30));
    }
}

