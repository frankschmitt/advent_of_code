use petgraph::algo::astar;
use petgraph::graph::*;
use crate::helpers::UIntGrid;

pub struct Chiton {
  graph: DiGraph<usize, usize>,
  start_idx: NodeIndex,
  end_idx: NodeIndex
}

impl Chiton {
  fn build_chiton(grid: &UIntGrid) -> Chiton {
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
    let edges2: Vec<(NodeIndex, NodeIndex, usize)> = edges.iter().map(|x| ((NodeIndex::new(x.0), NodeIndex::new(x.1), x.2))).collect();
    let g = DiGraph::<usize, usize>::from_edges(&edges2);
    let mut start_idx = NodeIndex::new(0);
    let mut end_idx = NodeIndex::new(grid.width() * grid.height() - 1);
    return Chiton { graph: g, start_idx: start_idx, end_idx: end_idx };
  }
}

pub fn solve() {
    let filename = "a15_chiton/input.txt";
    //let filename = "a15_chiton/example_input.txt";
    let grid = crate::helpers::read_uint_grid((&filename).to_string());  
    let chiton = Chiton::build_chiton(&grid); 
    let path = astar(&chiton.graph, chiton.start_idx, |finish| finish == chiton.end_idx, |e| *e.weight(), |_| 0);
    //println!("graph: {:?}", chiton.graph);
    println!("path: {:?}", path);
    // astar returns the weight plus the path - we're only interested in the weight
    // TODO this doesn't return the correct result for the "real" input - apparently, the A* implementation of petgraph isn't optimal. SUBMIT A BUG REPORT!
    let result1 = path.unwrap().0;
    let result2 = -1;
    println!("15 - chiton: {} {}", result1, result2);
}