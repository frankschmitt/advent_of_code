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
  macro_rules! c3 {
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

}

mod a01_report_repair {
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
                    if (x+y+z == 2020) {
                        result2 = x*y*z;
                    }
                }
            }
        }

        println!("01 - report repair: {} {}", result1, result2);
    }
}

mod a02_password_philosophy {
    pub fn solve() {
        let filename = "a02_password_philosophy/input.txt";
        let values = crate::helpers::read_string_list((&filename).to_string());
        println!("got values: {}", values.len());
        // a line of input consists of: <min>-<max> <letter>: <password>
        let re = Regex::new(r"(\d+)-(\d+) ([a-z]): ([a-z]+)").unwrap();
        let result1 = -1;
        let result2 = -1;
        println!("02 - password philsophy : {} {}", result1, result2);
    }
}
// pub use a01_report_repair;
fn main() {
    a01_report_repair::solve();
    a02_password_philosophy::solve();
}
