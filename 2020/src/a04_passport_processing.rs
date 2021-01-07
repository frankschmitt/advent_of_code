use std::str::FromStr;
use std::num::ParseIntError;
use std::collections::HashMap;
use std::iter::FromIterator;
use regex::Regex;

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
    
    // part I: a passport is valid if all fields are present (cid is optional, though)
    pub fn is_valid(&self) -> bool {
        return self.byr.is_some() &&
               self.iyr.is_some() &&
               self.eyr.is_some() &&
               self.hgt.is_some() &&
               self.hcl.is_some() &&
               self.ecl.is_some() &&
               self.pid.is_some();
    }

    fn is_u64_between_bounds(v: &str, min: u64, max: u64) -> bool {
      return match v.parse::<u64>() { 
          Err(_) => false,
          Ok(val) => val >= min && val <= max 
      };
    }  
      
    fn is_valid_height(v: &Option<String>) -> bool {
      let re = Regex::new(r"(\d{2,3})(in|cm)").unwrap();
      let result = match v {
        None => false,
        Some(x) => match re.captures(x) {
          None => false,
          // md[0] is the complete match - the captured groups start at 1
          Some(md) => (&md[2] == "in" && Passport::is_u64_between_bounds(&md[1], 59, 76)) ||
                      (&md[2] == "cm" && Passport::is_u64_between_bounds(&md[1], 150, 193))
        }
      };
      // println!("is_valid_height: {} -> {}", Passport::to_string(v), result);
      return result;
    }
    
    fn is_valid_rgb(v: &Option<String>) -> bool {
      let re = Regex::new(r"^#[0-9a-f]{6}$").unwrap();
      let result = match v {
        None => false,
        Some(x) => re.is_match(x)
      };
      // println!("is_valid_rgb: {} -> {}", Passport::to_string(v), result);
      return result;
    }
   
    fn to_string(v:&Option<String>) -> String {
      return match v {
        None => "None".to_string(),
        Some(x) => x.to_string()
      };
    }

    fn is_valid_colour_name(v: &Option<String>) -> bool {
      let result = match v {
        None => false,
        Some(x) => match x.as_ref() {
          "amb" => true,
          "blu" => true, 
          "brn" => true, 
          "gry" => true, 
          "grn" => true, 
          "hzl" => true, 
          "oth" => true,
          _ => false  
        }
      };
      // println!("is_valid_colour_name: {} -> {}", Passport::to_string(v), result);
      return result;
    }
   
    fn is_valid_pid(v: &Option<String>) -> bool {
      let re = Regex::new(r"^[0-9]{9}$").unwrap();
      let result = match v {
        None => false,
        Some(x) => re.is_match(x)
      };
      // println!("is_valid_pid: {} -> {}", Passport::to_string(v), result);
      return result;
    }

    fn is_valid_year_between_bounds(v: &Option<String>, min: u64, max: u64) -> bool {
      let result = match v {
        None => false,
        Some(x) => x.len() == 4 && Passport::is_u64_between_bounds(x, min, max)
      };
      // println!("is_valid_year_between_bounds: {}, {}, {} -> {}", Passport::to_string(v), min, max, result);
      return result;
    
    }

    // part II: a passport is valid if the fields satisfy these criteria
    // byr (Birth Year) - four digits; at least 1920 and at most 2002.
    // iyr (Issue Year) - four digits; at least 2010 and at most 2020.
    // eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
    // hgt (Height) - a number followed by either cm or in:
    //                  If cm, the number must be at least 150 and at most 193.
    //                  If in, the number must be at least 59 and at most 76.
    // hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
    // ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
    // pid (Passport ID) - a nine-digit number, including leading zeroes.
    // cid (Country ID) - ignored, missing or not.
    pub fn is_valid2(&self) -> bool {
      let is_valid_byr = Passport::is_valid_year_between_bounds(&self.byr, 1920, 2002);
      let is_valid_iyr = Passport::is_valid_year_between_bounds(&self.iyr, 2010, 2020);
      let is_valid_eyr = Passport::is_valid_year_between_bounds(&self.eyr, 2020, 2030);
      let is_valid_hgt = Passport::is_valid_height(&self.hgt);
      let is_valid_hcl = Passport::is_valid_rgb(&self.hcl);
      let is_valid_ecl = Passport::is_valid_colour_name(&self.ecl);
      let is_valid_pid = Passport::is_valid_pid(&self.pid);

      return is_valid_byr &&
             is_valid_iyr &&
             is_valid_eyr &&
             is_valid_hgt &&
             is_valid_hcl && 
             is_valid_ecl &&
             is_valid_pid;
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
    // ruleset2 is more strict than ruleset1, therefore, it suffices to only check the valid passports from part I
    let valid_passports2 = valid_passports.iter().filter( |&p| p.is_valid2()).collect::<Vec<_>>();
    let result2 = valid_passports2.len();
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

    fn read_test_input2() -> Vec<Passport> {
      let filename = "src/a04_passport_processing/test_input2.txt";
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

    #[test]
    fn test_input2_should_contain_four_valid2_passports() {
      let passports = read_test_input2();
      let valid_passports = passports.iter().filter( |&p| p.is_valid2()).collect::<Vec<_>>();
      assert_eq!(8, passports.len()) ;
      assert_eq!(4, valid_passports.len()) ;
    }

}

