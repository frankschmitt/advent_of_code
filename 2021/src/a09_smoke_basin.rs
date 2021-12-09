use std::str;

    pub fn solve() {
        let filename = "a09_smoke_basin/input.txt";
        // let filename = "a09_smoke_basin/example_input.txt";
        let grid = crate::helpers::read_uint_grid((&filename).to_string());
        //grid.print();

        let mut low_points = vec![];
        // TODO: write an iterator for this (incl. wraparound / without wraparound)        
        for y in 0 .. grid.height() {
            for x in 0 .. grid.width() {
                let val = grid.cell(y, x);
                // top neighbour (if we aren't in the first row)
                let smaller_than_top = (y == 0) || (val < grid.cell(y-1,x));
                // bottom neighbour (if we aren't in the last row)
                let smaller_than_bottom = (y == grid.height-1) || (val < grid.cell(y+1,x));
                // left neighbour (if we aren't in the first column)
                let smaller_than_left = (x == 0) || (val < grid.cell(y,x-1));
                // bottom neighbour (if we aren't in the last column)
                let smaller_than_right = (x == grid.width-1) || (val < grid.cell(y,x+1));
                if smaller_than_top && smaller_than_bottom && smaller_than_left && smaller_than_right {
                    low_points.push((y,x));
                } 
            }
        }
        //println!("low points: {:?}", low_points);
        let result1 = low_points.iter().map(|(y,x)| grid.cell(*y,*x) + 1).fold(0, |acc, val| acc + val);
        let result2 = -1;
        println!("09 smoke basin: {} {}", result1, result2);
}