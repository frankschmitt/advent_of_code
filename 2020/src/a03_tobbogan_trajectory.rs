pub fn solve() {
    struct Walk {
        x: usize,
        y: usize,
        delta_x: usize,
        delta_y: usize,
        trees: usize
    }

    // let filename = "a03_tobbogan_trajectory/test_input.txt";
    let filename = "a03_tobbogan_trajectory/input.txt";
    let grid = crate::helpers::read_char_grid(filename.to_string());
    let walks = vec![Walk { x:0, y:0, delta_x:1, delta_y : 1, trees : 0},
                     Walk { x:0, y:0, delta_x:3, delta_y : 1, trees : 0},
                     Walk { x:0, y:0, delta_x:5, delta_y : 1, trees : 0},
                     Walk { x:0, y:0, delta_x:7, delta_y : 1, trees : 0},
                     Walk { x:0, y:0, delta_x:1, delta_y : 2, trees : 0}
                    ];
    let walks2 = vec![ Walk { x:0, y:0, delta_x:3, delta_y : 1, trees : 0},
                    ];
    let mut result1 = 0;
    let mut result2 = 1;
    let mut pos = (0,0);
    for mut w in walks {
        println!("walking with {} / {}", w.delta_x, w.delta_y);
        while w.y < grid.height() {
            let c = grid.cell(w.y, w.x);
            if c == '#' {
                println!("tree");
                w.trees += 1;
            }
            else {
                println!("square");
            }
            w.x += w.delta_x;
            w.y += w.delta_y;
        }
        if w.delta_x == 3 {
            result1 = w.trees;
        }
        result2 = result2 * w.trees;
    }

    println!("grid dim: height = {}, width = {}", grid.height(), grid.width());
    println!("03 - tobbogan trajectory: {} {}", result1, result2);
}
