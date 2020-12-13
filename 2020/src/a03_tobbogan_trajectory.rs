pub fn solve() {
    // let filename = "a03_tobbogan_trajectory/test_input.txt";
    let filename = "a03_tobbogan_trajectory/input.txt";
    let grid = crate::helpers::read_char_grid(filename.to_string());
    let mut result1 = 0;
    let mut result2 = -1;
    let mut pos = (0,0);
    for i in 0 .. grid.height() {
        let c = grid.cell(pos.0,pos.1);
        if c == '#' {
            println!("tree");
            result1 += 1;
        }
        else {
            println!("square");
        }
        pos.0 += 1;
        pos.1 += 3;
    }

    println!("grid dim: height = {}, width = {}", grid.height(), grid.width());
    println!("03 - tobbogan trajectory: {} {}", result1, result2);
}
