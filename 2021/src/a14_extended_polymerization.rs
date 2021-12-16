use regex::Regex;
use std::collections::HashMap;

#[derive(Debug, Eq, PartialEq, Ord, PartialOrd, Clone)]
pub struct Rule {
    source: (char, char),
    target: char
}

#[derive(Debug, Eq, PartialEq)]
pub struct Polymerization {
   formula: Vec<char>,
   rules: Vec<Rule>,
   digraphs: HashMap<(char, char), i128>,
   characters: HashMap<char, i128>
}

impl Polymerization {

    pub fn new(formula: &Vec<char>, rules: &Vec<Rule>) -> Polymerization {
        let mut digraphs: HashMap<(char, char), i128> = HashMap::new();
        for win in formula.windows(2) {
            let key: (char, char) = (win[0], win[1]);
            *digraphs.entry(key).or_insert(0) += 1;
        }
        let characters = formula.iter().fold(HashMap::new(), |mut acc, ch| {
            *acc.entry(*ch).or_insert(0) += 1;
            acc
        });
        
        return Polymerization { formula: formula.to_vec(), rules: rules.to_vec(), digraphs: digraphs, characters: characters };
    }

    pub fn step(&mut self) {
      // old style
      /*let mut idx = 0;
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
      }*/
      // new style
      let mut new_digraphs: HashMap<(char, char), i128> = HashMap::new();
      let mut new_chars: HashMap<char, i128> = HashMap::new();
      for d in self.digraphs.iter() {
        for r in self.rules.iter() {
            if d.0.0 == r.source.0 && d.0.1 == r.source.1 {
              *new_digraphs.entry((r.source.0, r.target)).or_insert(0) += d.1;
              *new_digraphs.entry((r.target, r.source.1)).or_insert(0) += d.1;
              *new_digraphs.entry((r.source.0, r.source.1)).or_insert(0) -= d.1; // remove the old pair
              *new_chars.entry(r.target).or_insert(0) += d.1;  
              // println!("matched rule: {:?} for {:?}\n", r, d);
              // println!("  new digraphs after: {:?}\n", new_digraphs);
            }
        }
      }
      for (d, count) in new_digraphs {
          *self.digraphs.entry(d).or_insert(0) += count;
      }
      for (ch, count) in new_chars {
          *self.characters.entry(ch).or_insert(0) += count;
      }
    }

    pub fn run(&mut self, num_steps: usize) {
        for i in 0 .. num_steps {
            self.step();
            //println!("step: {}, formula: {:?}", i, self.formula);
        }
    }

    // get the distance, i.e. the number of occurrences of the most frequent element minus the number of occurrences of the least frequent element
    pub fn distance(&self) -> i128 {
        /*let hmap = self.formula.iter().fold(HashMap::new(), |mut acc, ch| {
            *acc.entry(ch).or_insert(0) += 1;
            acc
        });*/
        let max = self.characters.iter().map(|(k,v)| v).max().unwrap();
        let min = self.characters.iter().map(|(k,v)| v).min().unwrap();
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
    return Polymerization::new(&formula, &rules);
}

pub fn solve() {
    //let filename = "a14_extended_polymerization/example_input.txt";
    let filename = "a14_extended_polymerization/input.txt";
    let v = crate::helpers::read_string_list((&filename).to_string());   
    // part I, old approach: scan the whole string, replace with *lowercase* characters (to avoid affecting later rules in the same iteration),
    //            and at the end of the step, replace all lowercase characters with uppercase
    //            presumably, this will fail spectacularly in part II because we scan the string multiple times
    let mut polymerization = parse_polymerization(&v);
    polymerization.run(10);
    let result1 = polymerization.distance();
    
    // idea for part II:
    //   * store pairs of characters + count in a hashmap ("Windows" of size 2 into the formula)
    //   * store characters and counts in a hashmap
    //   * in each step:
    //     * check each rules against the hashmap of pairs
    //     *   if the rule matches: increase count of the target character
    //     *                        increase count of both pairs (source1, target) and (target, source2) 
    //     *   then, at the end, we just have to return max and min of our hashmap for the character counts
    polymerization.run(30);
    let result2 = polymerization.distance();
    println!("14 - extended polymerization: {} {}", result1, result2);
}
