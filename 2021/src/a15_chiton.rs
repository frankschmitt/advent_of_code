use petgraph::algo::astar;
use petgraph::graph::*;
use crate::helpers::UIntGrid;

fn build_graph(grid: &UIntGrid) -> DiGraph<i32, i32> {
  /*let mut g = Graph::new();
  let mut nodes = vec![];
  for row in 0 .. grid.height {
      for col in 0 .. grid.width {
          nodes.append(g.add_node(format!(grid.cells(row, col))));
      }
  }
  return g;*/

  // List of edges: <starg, end, weight>
  let mut edges: Vec<(usize, usize, usize)> = vec![];
  // build a directed graph; the edge weight is equal to the weight of the end node (the "cost" of going to that node)
  for y in 0 .. grid.height() {
    for x in 0 .. grid.width() {
        // add edge to right neighbour and vice versa
        let curr_node = y*grid.width() + x;
        if x < grid.width()-1 {
          let right = y*grid.width() + (x+1);
          edges.push((curr_node, right, grid.cell(y, x)));
          edges.push((right, curr_node, grid.cell(y, x+1)));
          
        }
        // add edge to bottom neighbour if it isn't a "mountain"
        if (y < grid.height()-1) && (grid.cell(y+1,x) != 9) {
          let bottom = (y+1)*grid.width() + x;
            edges.push((curr_node, bottom, grid.cell(y, x)));
            edges.push((bottom, curr_node, grid.cell(y+1, x)));    
        }
    }
}
// build graph
let edges2: Vec<(NodeIndex, NodeIndex)> = edges.iter().map(|x| (NodeIndex::new(x.0), NodeIndex::new(x.1))).collect();
let g = DiGraph::<i32, i32>::from_edges(&edges2);
return g;
}

pub fn solve() {
    //let filename = "a15_chiton/input.txt";
    let filename = "a15_chiton/example_input.txt";
    let grid = crate::helpers::read_uint_grid((&filename).to_string());  
    let g = build_graph(&grid); 
    // TODO: build_graph must return the start and finish node, as well
    let path = astar(&g, a, |finish| finish == f, |e| *e.weight(), |_| 0);
    assert_eq!(path, Some((6, vec![a, d, e, f])));
    let result1 =-1;
    let result2 = -1;
    println!("15 - chiton: {} {}", result1, result2);
}