
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

     // let contents = fs::read_to_string(filename)
       //  
        //                 .expect("Something went wrong reading the file");
     //let mut result = Vec::new();
     //Ok(result)
     //let path = Path::new(&filename);
     //let mut file = BufferedReader::new(File::open(&path));
     //let result: Vec<i64> = file.lines().map(|x| x.into());
     return result;
  }

}

mod a01_report_repair {
    pub fn solve() {
        println!("hello from a01");
        let filename = "a01_report_repair/input.txt";
        let values = crate::helpers::read_int_list((&filename).to_string());
        println!("got values: {}", values.len());
    }
}

// pub use a01_report_repair;
fn main() {
    println!("Hello, world!");
    a01_report_repair::solve();
}
