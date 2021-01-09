use std::collections::HashSet;
use std::num::ParseIntError;
use std::str::FromStr;

#[derive(Debug, PartialEq)]
pub struct Group {
    positive_answers: HashSet<char>
}

impl Group {
    pub fn count(&self) -> usize {
        return self.positive_answers.len();
    }
}

impl FromStr for Group {
    type Err = ParseIntError;
  
    fn from_str(s: &str) -> Result<Self, Self::Err> {
          // entries are separated by spaces
          //let field_strings: Vec<&str> = s.split_ascii_whitespace().collect::<Vec<_>>();
          let chars = s.chars().filter(|ch| ch.is_ascii_alphabetic());
          let mut positive_answers = HashSet::<char>::new();
          chars.for_each(|ch| { 
              let _ = positive_answers.insert(ch);
          }); 
          return Ok( Group { 
            positive_answers: positive_answers
          });
    }
} 
  
pub fn read_input(filename: &str) -> Vec<Group> {
    let input: String = crate::helpers::read_string((&filename).to_string());
    // empty lines delimit records; split the input, and parse the records into a vector of passports
    let values: Vec<&str> = input.split("\n\n").collect::<Vec<_>>();
    let result: Vec<Group> = values.iter().map(|s| s.parse().unwrap()).collect();
    return result;
}

pub fn sum_count(groups: &Vec<Group>) -> usize {
    return groups.iter().fold(0, |acc, g| acc + g.count());
}

pub fn solve() {
    let filename = "a06_custom_customs/input.txt";
    let groups = read_input(filename);
    let result1 = sum_count(&groups);
    let result2 = -1;
    println!("06 - custom customs: {} {}", result1, result2);
}


#[cfg(test)]
mod tests {

    use crate::a06_custom_customs::*;

    fn read_test_input() -> Vec<Group> {
      let filename = "src/a06_custom_customs/test_input.txt";
      let groups = read_input(filename);
      return groups;
    }

    #[test]
    fn test_input_should_contain_five_groups() {
      let groups = read_test_input();
      assert_eq!(5, groups.len());
    }

    #[test]
    fn test_input_should_give_sum_count_eleven() {
        let groups = read_test_input();
        assert_eq!(11, sum_count(&groups));
    }
}