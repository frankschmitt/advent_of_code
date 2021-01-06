use std::str::FromStr;
use std::num::ParseIntError;
use std::collections::HashMap;
use std::iter::FromIterator;

#[derive(Debug, PartialEq)]
pub struct Passport {
    byr: Option<String>, // (Birth Year)
    iyr: Option<String>, // (Issue Year)
    eyr: Option<String>, //  (Expiration Year)
    hgt: Option<String>, // (Height), given either in inches or cm
    hcl: Option<String>, //  (Hair Color)
    ecl: Option<String>, //  (Eye Color)
    pid: Option<String>, // (Passport ID)
    cid: Option<String>  // (Country ID)
}

impl FromStr for Passport {
  type Err = ParseIntError;

  fn from_str(s: &str) -> Result<Self, Self::Err> {
     /* let coords: Vec<&str> = s.trim_matches(|p| p == '(' || p == ')' )
                               .split(',')
                               .collect();

      let x_fromstr = coords[0].parse::<i32>()?;
      let y_fromstr = coords[1].parse::<i32>()?;

      Ok(Point { x: x_fromstr, y: y_fromstr })*/
        // fields are separated by spaces
        let field_strings: Vec<&str> = s.split_ascii_whitespace().collect::<Vec<_>>();
        // fields consist of <name>:<value>
        let fields: HashMap<String, String> = HashMap::from_iter(
                                  field_strings.iter()
                                  .map(|f| f.split(':').collect::<Vec<_>>()) // split at :; this gives us a vector of strings
                                  .map(|v| (v[0].to_string(), v[1].to_string())) // extract key-value pairs
        );
        return Ok( Passport { 
          byr: fields.get("byr").map(|s| s.to_string()), 
          iyr: fields.get("iyr").map(|s| s.to_string()),  
          eyr: fields.get("eyr").map(|s| s.to_string()),  
          hgt: fields.get("hgt").map(|s| s.to_string()), 
          hcl: fields.get("hcl").map(|s| s.to_string()),  
          ecl: fields.get("ecl").map(|s| s.to_string()),  
          pid: fields.get("pid").map(|s| s.to_string()),  
          cid: fields.get("cid").map(|s| s.to_string())});
      } 
}

impl Passport {
    
    pub fn is_valid(&self) -> bool {
        return self.byr.is_some() &&
               self.iyr.is_some() &&
               self.eyr.is_some() &&
               self.hgt.is_some() &&
               self.hcl.is_some() &&
               self.ecl.is_some() &&
               self.pid.is_some();
    }

}

pub fn read_input(filename: &str) -> Vec<Passport> {
    let input: String = crate::helpers::read_string((&filename).to_string());
    // empty lines delimit records; split the input, and parse the records into a vector of passports
    let values: Vec<&str> = input.split("\n\n").collect::<Vec<_>>();
    let result: Vec<Passport> = values.iter().map(|s| s.parse().unwrap()).collect();
    return result;
}

pub fn solve() {
    let filename = "a04_passport_processing/input.txt";
    let passports = read_input(filename);

    let valid_passports = passports.iter().filter( |&p| p.is_valid()).collect::<Vec<_>>();
    let result1 = valid_passports.len();
    let result2 = -1;
    println!("04 - passport processing: {} {}", result1, result2);
}


#[cfg(test)]
mod tests {

    use crate::a04_passport_processing::Passport;
    use crate::a04_passport_processing::read_input;

    fn read_test_input() -> Vec<Passport> {
      let filename = "src/a04_passport_processing/test_input.txt";
      let passports = read_input(filename);
      return passports;
    }


    #[test]
    fn test_input_should_contain_four_passports() {
      let passports = read_test_input();
      assert_eq!(4, passports.len());
    }

    #[test]
    fn test_input_should_contain_two_valid_passports() {
      let passports = read_test_input();
      let valid_passports = passports.iter().filter( |&p| p.is_valid()).collect::<Vec<_>>();
      assert_eq!(2, valid_passports.len()) ;
    }


}

