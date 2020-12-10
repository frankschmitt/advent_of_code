#[macro_use(c)]
extern crate cute;
extern crate regex;

mod helpers {
  //use std::fs;
  //use std::io;
  //use std::path::Path;
  use std::io::{BufRead, BufReader};
  use std::fs::File;

  pub fn read_int_list(filename: String) -> Vec<i64> {
      let reader = BufReader::new(File::open(filename).expect("Cannot open file"));
      let mut result = Vec::new();
      let mut val: i64;
      for line in reader.lines() {
          val = line.unwrap().parse().unwrap();
          result.push(val);
      }

     return result;
  }

  pub fn read_string_list(filename: String) -> Vec<String> {
      let reader = BufReader::new(File::open(filename).expect("Cannot open file"));
      let mut result = Vec::new();
      let mut val: String;
      for line in reader.lines() {
          val = line.unwrap();
          result.push(val);
      }

     return result;
  }

  // cute doesn't provide a version for three iterators 
  /* macro_rules! c3 {
    ($exp:expr, for $i:ident in $iter:expr, for $i2:ident in $iter2:expr, for $i3:ident in $iter3:expr, if $cond:expr) => (
        {
            let mut r = vec![];
            for $i3 in $iter3 {
                for $i2 in $iter2 {
                    for $i in $iter {
                        if $cond{
                            r.push($exp);
                        }
                    }
                }
            }
            r
        }
    );
  }
  */

}

mod a01_report_repair {
    // detect all pairs (part 1) / triples (part 2) in the input whose sum equals 2020, and compute
    // their product
    pub fn solve() {
        let filename = "a01_report_repair/input.txt";
        let values = crate::helpers::read_int_list((&filename).to_string());
        // println!("got values: {}", values.len());
        let result1 = c![x*y, for x in &values, for y in &values, if x+y  == 2020 && x > y][0];
        // cute doesn't provide 3-iterator versions - therefore, use simple loops here
        let mut result2:i64 = -1;
        for x in &values {
            for y in &values {
                for z in &values {
                    if x+y+z == 2020 {
                        result2 = x*y*z;
                    }
                }
            }
        }

        println!("01 - report repair: {} {}", result1, result2);
    }
}

mod a02_password_philosophy {
    use regex::Regex;

    // day 2 is pretty straightforward: parse each line, count the number of matching letters in
    // the substring (part 1) or at positions min/max (day2), and increment the counter if the hit
    // count equals 1
    pub fn solve() {
        let filename = "a02_password_philosophy/input.txt";
        let values = crate::helpers::read_string_list((&filename).to_string());
        // println!("got values: {}", values.len());
        // a line of input consists of: <min>-<max> <letter>: <password>
        let re = Regex::new(r"(\d+)-(\d+) ([a-z]): ([a-z]+)").unwrap();
        /* let re = Regex::new(r"(?x)
(?P<min>\d+)  # min number of occurrences
-
(?P<max>\d+) # max number of occurrences

(?P<letter>[a-z])   # the letter
:
 
(?P<password>[a-z]+) # the password
").unwrap(); */
        let mut result1: usize = 0;
        let mut result2: usize = 0;
        for line in values {
          // println!("parsing line {}", &line);
          let md = re.captures(&line).unwrap();
          // println!("found entry, min = {}, max = {}, letter = {}, password = {}", &md["min"], &md["max"], &md["letter"], &md["password"]);
          let min: usize = md[1].parse().unwrap();
          let max: usize = md[2].parse().unwrap();
          let ch: char = md[3].chars().nth(0).unwrap();
          let password: String = md[4].to_string();
          // println!("found entry, min = {}, max = {}, letter = {}, password = {}", &md[1], &md[2], &md[3], &md[4]);
          // part 1: count the number of occurrences in the password overall
          let hit_count1 = password.chars().filter(|x| x == &ch).count();
          // part 2: count the number of occurrences in the slice from min to max (1-based); must
          // be exactly 1!
          let hit_count2 = password[min-1 .. min].chars().filter(|x| x == &ch).count() + 
                           password[max-1 .. max].chars().filter(|x| x == &ch).count();
          // println!("found entry, min = {}, max = {}, letter = {}, password = {}, hit_count1 = {}, hit_count2 = {}", &min, &max, &ch, &password, &hit_count1, &hit_count2);
          if (min <= hit_count1) && (hit_count1 <= max) {
              result1 += 1;
          }
          if hit_count2 == 1 {
              result2 += 1;
          }
        }
        println!("02 - password philosophy : {} {}", result1, result2);
    }
}


mod a09_encoding_error {

    pub fn solve() {
        let filename = "a09_encoding_error/input.txt";
        let values = crate::helpers::read_int_list((&filename).to_string());
        // part 1: iterate over slices of length 25, and for all pairs within that slice, check
        // whether the sum equals the current value
        let mut idx = 25; // we start at index 25
        let result1 = loop {
          let found = c![x*y, for x in &values[idx-25 .. idx], for y in &values[idx-24 .. idx], 
                              if x+y  == values[idx]].len() > 0;
          if found == false {
            break &values[idx];
          }
          idx += 1;
        };
        // part 2: find the sub-range whose sum equals result1, 
        //   and keep track of our sub-range by using two indices
        let mut i = 0;
        let mut j = 0;
        let mut sum = values[i];
        let _range2 = loop {
            if sum == *result1 {
                break (i,j);
            }
            else if sum > *result1 {
                sum -= values[i];
                i += 1;
            }
            else {
                j += 1;
                sum += values[j];
            }
        }; 
        // now, find the min and max in this range
        let min = values[i .. j+1].iter().min().unwrap();
        let max = values[i .. j+1].iter().max().unwrap();
        let result2 = min + max;
        println!("09 - encoding error: {} {}", result1, result2);
    }

}

// pub use a01_report_repair;
fn main() {
    a01_report_repair::solve();
    a02_password_philosophy::solve();
    a09_encoding_error::solve();
}
