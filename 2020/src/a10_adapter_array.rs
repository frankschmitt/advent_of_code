use maplit::hashmap;
use std::collections::{HashMap};
use std::vec::Vec;
use std::iter::FromIterator;
use petgraph::graph::{DiGraph, NodeIndex};

pub struct JoltProblem {
    adapters: Vec<i64>, 
    graph: DiGraph<i64, i64>,
    start_idx: NodeIndex,
    end_idx: NodeIndex
}

/// compute all jolt differences between neighbouring adapters
/// @returns a hashmap whose keys are the jolt differences and whose values are the number of occurrences of these differences
pub fn get_jolt_differences(problem: &JoltProblem) -> HashMap<i64, u64> {
    let mut result = HashMap::<i64, u64>::new();
    problem.adapters.windows(2).for_each(|w|
        {
            let diff = w[1] - w[0];
            *result.entry(diff).or_insert(0) += 1;
        }
    );
    return result;
}

pub fn solve() {
    let problem = read_input("a10_adapter_array/input.txt");
    let differences = get_jolt_differences(&problem);
    let result1 = differences[&1] * differences[&3];
    let result2 = get_path_count(&problem);
    println!("10 - adapter array: {} {}", result1, result2);
}


/// given the list of (sorted) adapters, build the connection graph
pub fn build_graph(adapters: &Vec<i64>) -> (DiGraph<i64, i64>, NodeIndex, NodeIndex) {
    let mut graph = DiGraph::<i64, i64>::new();
    let mut nodes_helper = HashMap::<i64, NodeIndex>::new(); // contains adapter -> NodeIndex mappings
    // add vertices for the adapters
    adapters.iter().for_each(|a|
        {
            let idx = graph.add_node(*a);
            nodes_helper.insert(*a, idx);
        }
    );
    // add edges - every viable combination (delta <= 3) gets an edge
    let mut current: usize = 0;
    let mut num_paths: usize = 0;
    while current < adapters.len() {
        println!("current: {}, #paths: {}", current, num_paths);
        let jolt0 = adapters[current];
        let node_idx0 = nodes_helper[&jolt0];
        let idx0 = current;
        let idx1 = idx0 + 1;
        let idx2 = idx0 + 2;
        let idx3 = idx0 + 3;
        // first combo compatible?
        if idx1 < adapters.len() {
            let jolt1 = adapters[idx1];
            if jolt0 + 3 >= jolt1 {
                let node_idx1 = nodes_helper[&jolt1];
                graph.add_edge(node_idx0, node_idx1, jolt1 - jolt0);
                num_paths += 1;
            }
        } 
        // second combo compatible?
        if idx2 < adapters.len() {
            let jolt2 = adapters[idx2];
            if jolt0 + 3 >= jolt2 {
                let node_idx2 = nodes_helper[&jolt2];
                graph.add_edge(node_idx0, node_idx2, jolt2 - jolt0);
                num_paths += 1;
            }
        } 
        // third combo compatible?
        if idx3 < adapters.len() {
            let jolt3 = adapters[idx3];
            if jolt0 + 3 >= jolt3 {
                let node_idx3 = nodes_helper[&jolt3];
                graph.add_edge(node_idx0, node_idx3, jolt3 - jolt0);
                num_paths += 1;
            }
        } 
        
        current = current + 1;    
    }
    let start_idx = nodes_helper[adapters.first().unwrap()];
    let end_idx = nodes_helper[adapters.last().unwrap()];
    println!("fin, returning graph");
    return (graph, start_idx, end_idx);
}

pub fn read_input(filename: &str) -> JoltProblem {
    let mut  adapters = crate::helpers::read_int_list(filename.to_string());
    adapters.push(0); // add the power outlet
    adapters.sort();
    let max = adapters.iter().max().unwrap();
    adapters.push(max + 3); // add the universal adapter
    let (graph, start_idx, end_idx) = build_graph(&adapters);
    return JoltProblem { adapters: adapters, graph: graph, start_idx: start_idx, end_idx: end_idx };
}

pub fn get_path_count(problem: &JoltProblem) -> usize {
    let mut result: usize = 0;
    println!("checking paths, #edges: {}", &problem.graph.edge_count());
    let paths: Vec<Vec<NodeIndex>> = petgraph::algo::all_simple_paths(&problem.graph, problem.start_idx, problem.end_idx, 0, None).collect();
    return paths.len();
}

#[cfg(test)]
mod tests {
    use crate::a10_adapter_array::*;

    fn read_test_input() -> JoltProblem {
        let filename = "src/a10_adapter_array/test_input.txt";
        return read_input(&filename);
    } 

    #[test]
    fn number_of_differences_for_test_input() {
        let problem = read_test_input();
        let expected = hashmap!{
            1 => 22,
            3 => 10,
        };
        
        assert_eq!(expected, get_jolt_differences(&problem));
    }

    #[test]
    fn number_of_paths_for_test_input() {
        let problem = read_test_input();
        assert_eq!(19208, get_path_count(&problem));
    }
}