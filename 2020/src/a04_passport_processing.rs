pub struct Passport {
    byr: Option<u32>, // (Birth Year)
    iyr: Option<u32>, // (Issue Year)
    eyr: Option<u32>, //  (Expiration Year)
    hgt: Option<String>, // (Height), given either in inches or cm
    hcl: Option<String>, //  (Hair Color)
    ecl: Option<String>, //  (Eye Color)
    pid: Option<String>, // (Passport ID)
    cid: Option<String> // (Country ID)
}

impl Passport {

    pub fn is_valid(&self) -> bool {
        return true;
    }
}

pub fn read_input(filename: &str) -> Vec<Passport> {
    let mut result = vec![];
    let input: String = crate::helpers::read_string((&filename).to_string());
    let values: Vec<&str> = input.split("").collect::<Vec<_>>();
    // let v: Vec<&str> = "Mary had a little lamb".split(' ').collect();
    for s in values {
        if s == "" {
          result.push(Passport { byr: None, iyr: None, eyr: None, hgt: None,
                                 hcl: None, ecl: None, pid: None, cid: None} );
        }
    }
    return result;
}

pub fn solve() {
    let filename = "a04_passport_processing/input.txt";
    let passports = read_input(filename);

    let result1 = -1;
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
    fn test_input_should_contain_three_passports() {
      let passports = read_test_input();
      assert_eq!(3, passports.len());
    }

    #[test]
    fn test_input_should_contain_two_valid_passports() {
      let passports = read_test_input();
      let valid_passports = passports.iter().filter( |&p| p.is_valid()).collect::<Vec<_>>();
      assert_eq!(2, valid_passports.len()) ;
    }


}

