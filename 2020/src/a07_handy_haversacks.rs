use petgraph::stable_graph::{StableDiGraph, NodeIndex};
use petgraph::dot::{Dot, Config};
use petgraph::algo::has_path_connecting;
use petgraph::visit::{EdgeRef};
use petgraph::Outgoing;
use std::collections::HashMap;
use std::fmt::Debug;
use regex::Regex;

pub struct BagPuzzle {
    nodes: HashMap<String, NodeIndex>,
    graph: StableDiGraph<String, usize>,
    shiny_node_idx: NodeIndex
}

pub struct EdgeDefinition {
    src: String,
    target: String,
    count: usize
}

pub enum ParseLineError {
    ParseLineError
}

// read input, and return a BagPuzzle.
// We proceed like this:
//   - read input line-wise into a vec
//   - split each input line at the comma - this gives us the first part (containing bag plus first contained bags) plus an arbitrary number of additional parts
//   - parse the first part; store all bag colours we encounter (these will become our nodes) plus the relation (these will become edges)
//   - parse all remaining parts; again, store the bag colours and the relations
//   - after parsing, create a graph, add the unique colours as nodes, and store them with their node indices
//   - add the edges 
pub fn read_input(filename: &str) -> BagPuzzle {
    let lines = crate::helpers::read_string_list((&filename).to_string());
    let mut edge_definitions: Vec<EdgeDefinition> = vec![];
    let mut node_definitions: Vec<String> = vec![];
    // Regex for first part: source "contains" first target|no other
    let re1 = Regex::new("([a-z ]+?) bags contain (([0-9]+) ([a-z ]+)|no other) bag").unwrap();
    // Regex for the following targets
    let re_target = Regex::new("([0-9]+) ([a-z ]+) bag").unwrap();  
    // read input line by line, parse it, build node list, and build the edge definition list
    for line in lines {
        // println!("checking line: {}", line);
        let v: Vec<&str> = line.split(",").collect(); // the first element contains the src node plus the first target node (or "no other")
        match re1.captures(v[0]) {
            None => println!("  {} -> no match!", v[0]),
            // md[0] is the complete match - the captured groups start at 1
            Some(md) => {
                let src = &md[1];
                // println!("  {} -> match: src = {}, target = {}", v[0], src, &md[2]);
                node_definitions.push(src.to_string());
                match &md[2] {
                    "no other" => (),
                    _ => {
                           //println!("  cnt = {}, type = {}", &md[3], &md[4]);
                           node_definitions.push(md[4].to_string());
                           edge_definitions.push(EdgeDefinition { src: src.to_string(), target: md[4].to_string(), count: md[3].parse().unwrap() })
                        }
                }
                // add nodes and edges for the 2nd, 3rd, ... edge on the current line
                for i in 1 .. v.len() {
                    // println!("  another match: {}", v[i]);
                    match re_target.captures(v[i]) {
                        None => println!("    {} -> no match!", v[i]),
                        Some(md2) => {
                            // println!("  cnt = {}, type = {}", &md2[1], &md2[2]);
                            node_definitions.push(md2[2].to_string());
                            edge_definitions.push(EdgeDefinition { src: src.to_string(), target: md2[2].to_string(), count: md2[1].parse().unwrap() })   
                        }
                    }
                }
            }
        }
    }
    // build hashmap from plain node list; we'll use this for building the edges 
    node_definitions.sort();
    node_definitions.dedup();
    let mut nodes = HashMap::<String, NodeIndex>::new();
    let mut graph = StableDiGraph::<String, usize>::new();
    let mut shiny_node_idx: NodeIndex = NodeIndex::new(0);
    for nd in node_definitions {
        let idx = graph.add_node(nd.to_string());
        nodes.insert(nd.to_string(), idx);
        if nd == "shiny gold" {
            shiny_node_idx = idx;
        }
    }
    // build edge list - we use the node indices from our nodes hashmap
    for ed in edge_definitions.iter() {
            let v1 = nodes.get(&ed.src).unwrap();
            let v2 = nodes.get(&ed.target).unwrap();
            graph.add_edge(*v1, *v2, ed.count);
    }
    let result = BagPuzzle { nodes: nodes, graph: graph, shiny_node_idx };
    return result;
}

pub fn get_node_count_connecting_to(g: &StableDiGraph<String, usize>, end_idx: NodeIndex) -> usize {
    let result = g.node_indices().filter( |&n|
        // note the innocuous &graph - figuring this out took me literally several hours :-(
        has_path_connecting(&g, n, end_idx, None)
    ).count() - 1; // ignore the connection from v1 to itself
    return result;
}



// part II: compute the total number of bags contained in a shiny gold bag
pub fn get_edge_weight_sum_from(g: &StableDiGraph<String, usize>, idx: NodeIndex) -> usize {
    // visit each outgoing edge; the edge weight for this branch is 
    //   the weight of the edge (= number of contained bags) times the weight of the target node
    //   the weight of the target node is its edge weight sum (the recursive call) plus one (the bag itself)
    let result = g.edges_directed(idx, Outgoing).map(|e| e.weight() * (1 + get_edge_weight_sum_from(g, e.target()))).sum();
    return result;
}

// this is a typical graph problem - the bag types are the vertices,
// and the contains relations are the edges (tagged with the number of bags)
// Therefore, we use this overall approach:
//  - build the complete list of bag types; these are our vertices
//  - build a list of all "a contains b" relations these are our edges
//  - find all paths that end in the shiny golden bag vertex
pub fn solve() {
    let filename = "a07_handy_haversacks/input.txt";
    let bp = read_input(filename);
    let graph = &bp.graph;
    // part I: compute the number of bags that might containg a shiny gold bag
    //         this is simple - just check all vertices whether they have a path leading to the shiny gold bag vertex
    let result1 = get_node_count_connecting_to(&graph, bp.shiny_node_idx);
    let result2 = get_edge_weight_sum_from(&graph, bp.shiny_node_idx);
    println!("07 - handy haversacks: {} {}", result1, result2);
}

#[cfg(test)]
mod tests {
    use crate::a07_handy_haversacks::*;

    fn read_test_input() -> BagPuzzle {
        let filename = "src/a07_handy_haversacks/test_input.txt";
        let graph = read_input(filename);
        return graph;
    }

    #[test]
    fn graph_for_test_input_should_be_built_correctly() {
        let bp = read_test_input();
        let g = bp.graph;
        // println!("{:?}", Dot::with_config(&g, &[]));
        assert_eq!(9, g.node_count());
        assert_eq!(13, g.edge_count());
    }

    #[test]
    fn node_count_connected_to_end_node_should_be_correct_for_test_input() {
        let bp = read_test_input();
        let g = bp.graph;
        assert_eq!(4, get_node_count_connecting_to(&g, bp.shiny_node_idx));
    }

    #[test]
    fn edge_weight_sum_from_start_node_should_be_correct_for_test_input() {
        let bp = read_test_input();
        let g = bp.graph;
        assert_eq!(32, get_edge_weight_sum_from(&g, bp.shiny_node_idx));
    }

}