use std::str;
use petgraph::algo::kosaraju_scc;
use petgraph::graph::*;
use std::cmp::Reverse;

pub fn solve() {
    let filename = "a09_smoke_basin/input.txt";
    // let filename = "a09_smoke_basin/example_input.txt";
    let grid = crate::helpers::read_uint_grid((&filename).to_string());

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
    let result1 = low_points.iter().map(|(y,x)| grid.cell(*y,*x) + 1).fold(0, |acc, val| acc + val);
    
    // part 2, approach: build undirected graph, remove all nodes labeled 9, determine strongly connected components, get 3 largest ones
    let mut edges: Vec<(usize, usize)> = vec![];
    // build an undirected graph; since edges are bidirectional, we only have to add them for the right and bottom neighbours
    for y in 0 .. grid.height() {
        for x in 0 .. grid.width() {
            // is current cell a "mountain" ? continue
            if grid.cell(y,x) == 9 {
                continue;
            }
            // add edge to right neighbour if it isn't a "mountain"
            if (x < grid.width()-1) && (grid.cell(y,x+1) != 9) {
                edges.push((y*grid.width() + x, y*grid.width() + (x+1)));
            }
            // add edge to bottom neighbour if it isn't a "mountain"
            if (y < grid.height()-1) && (grid.cell(y+1,x) != 9) {
                edges.push((y*grid.width()+x, (y+1)*grid.width()+x));    
            }
        }
    }
    // build graph
    let edges2: Vec<(NodeIndex, NodeIndex)> = edges.iter().map(|x| (NodeIndex::new(x.0), NodeIndex::new(x.1))).collect();
    let g = UnGraph::<i32, ()>::from_edges(&edges2);
    // compute SCC's, get their sizes, sort 'em, take the first 3, and get the product
    let scc = kosaraju_scc(&g);
    let mut scc_sizes = scc.iter().map(|cc| cc.len()).collect::<Vec<_>>();
    scc_sizes.sort_by(|a, b| b.cmp(a));
    let result2 = scc_sizes.iter().take(3).fold(1, |acc, x| acc * x);

    println!("09 smoke basin: {} {}", result1, result2);
}