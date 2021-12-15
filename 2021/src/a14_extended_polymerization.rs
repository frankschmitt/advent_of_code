use std::collections::LinkedList;
use regex::Regex;
use std::collections::HashMap;

#[derive(Debug, Eq, PartialEq, Ord, PartialOrd)]
pub struct Rule {
    source: (char, char),
    target: char
}

#[derive(Debug, Eq, PartialEq, Ord, PartialOrd)]
pub struct Polymerization {
   formula: Vec<char>,
   rules: Vec<Rule>
}

impl Polymerization {
    pub fn step(&mut self) {
      let mut idx = 0;
      loop {
         for r in self.rules.iter() {
             if self.formula[idx] == r.source.0 && self.formula[idx+1] == r.source.1 {
                 self.formula.insert(idx+1, r.target);
                 idx = idx + 1; // skip the new element
                 break; // skip remaining rules
             }
         }
         idx = idx + 1;
         if idx == self.formula.len() -1 { break; }
      }
    }

    pub fn run(&mut self, num_steps: usize) {
        for i in 0 .. num_steps {
            self.step();
            //println!("step: {}, formula: {:?}", i, self.formula);
        }
    }

    // get the distance, i.e. the number of occurrences of the most frequent element minus the number of occurrences of the least frequent element
    pub fn distance(&self) -> usize {
        let hmap = self.formula.iter().fold(HashMap::new(), |mut acc, ch| {
            *acc.entry(ch).or_insert(0) += 1;
            acc
        });
        let max = hmap.iter().map(|(k,v)| v).max().unwrap();
        let min = hmap.iter().map(|(k,v)| v).min().unwrap();
        return max - min;
    }
}

pub fn parse_polymerization(input: &Vec<String>) -> Polymerization {
    let mut formula = vec![];
    let mut rules = vec![];
    for (idx, line) in input.iter().enumerate() {
      if idx == 0 {
        formula = line.chars().collect();
      }
      else if idx == 1 {
          continue;
      }
      else {
          rules.push(Rule { source: (line.chars().nth(0).unwrap(), line.chars().nth(1).unwrap()), target: line.chars().nth(6).unwrap() });          
      }
    }
    return Polymerization { formula: formula, rules: rules };
}

pub fn solve() {
    //let filename = "a14_extended_polymerization/example_input.txt";
    let filename = "a14_extended_polymerization/input.txt";
    let v = crate::helpers::read_string_list((&filename).to_string());   
    // part I, approach: scan the whole string, replace with *lowercase* characters (to avoid affecting later rules in the same iteration),
    //            and at the end of the step, replace all lowercase characters with uppercase
    //            presumably, this will fail spectacularly in part II because we scan the string multiple times
    let mut polymerization = parse_polymerization(&v);
    //println!("{:?}", polymerization);
    polymerization.run(10);
    //println!("len: {}", polymerization.formula.len());
    let result1 = polymerization.distance();
    
    // idea for part II:
    //   * store pairs of characters + count in a hashmap ("Windows" of size 2 into the formula)
    //   * store characters and counts in a hashmap
    //   * in each step:
    //     * check each rules against the hashmap of pairs
    //     *   if the rule matches: increase count of the target character
    //     *                        increase count of both pairs (source1, target) and (target, source2) 
    //     *   then, at the end, we just have to return max and min of our hashmap for the character counts
    let result2 = -1;
    println!("14 - extended polymerization: {} {}", result1, result2);
}
