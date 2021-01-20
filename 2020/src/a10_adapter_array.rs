use maplit::hashmap;
use std::collections::{HashMap};
use std::iter::FromIterator;

pub struct JoltProblem {
    adapters: Vec<i64>
}

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
    let result2 = -2;
    println!("10 - adapter array: {} {}", result1, result2);
}

pub fn read_input(filename: &str) -> JoltProblem {
    let mut  adapters = crate::helpers::read_int_list(filename.to_string());
    adapters.push(0); // add the power outlet
    adapters.sort();
    let max = adapters.iter().max().unwrap();
    adapters.push(max + 3); // add the universal adapter
    return JoltProblem { adapters: adapters };
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
}