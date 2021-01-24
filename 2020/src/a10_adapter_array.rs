use maplit::hashmap;
use std::collections::{HashMap};
use std::vec::Vec;
use std::iter::FromIterator;

pub struct JoltProblem {
    adapters: Vec<i64>
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

pub fn read_input(filename: &str) -> JoltProblem {
    let mut  adapters = crate::helpers::read_int_list(filename.to_string());
    adapters.push(0); // add the power outlet
    adapters.sort();
    let max = adapters.iter().max().unwrap();
    adapters.push(max + 3); // add the universal adapter
    return JoltProblem { adapters: adapters  };
}


/// Part II is a modified Fibonacci sequence - the number of paths from node X equals the sum of paths from its predecessors
/// each node has between 1 and 3 predecessors
pub fn get_path_count(problem: &JoltProblem) -> usize {
    let mut path_counts: Vec<usize> = vec![1, 1]; // initialize the first two entries for the outlet and the first adapter 
    for i in 2 .. problem.adapters.len()-1 {
        let mut its_result = 0;
        let adapters = &problem.adapters;
        if (i >= 3) && (adapters[i-3] >= adapters[i] -3) {
            its_result += path_counts[i-3];
        }
        if adapters[i-2] >= adapters[i] -3 {
            its_result += path_counts[i-2];
        }
        // the immediate predecessor is always compatible
        its_result += path_counts[i-1];
        path_counts.push(its_result);
    }
    return *path_counts.last().unwrap();
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