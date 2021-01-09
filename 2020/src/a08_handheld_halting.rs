use core::str::FromStr;
use core::num::ParseIntError;

#[derive(Debug, PartialEq)]
pub enum Instruction {
    Acc(i64),
    Jmp(i64),
    Nop
}

pub struct Program {
    instructions: Vec<Instruction>,
    accumulator: i64, 
    next: usize // index of the next instruction to execute
}

impl Program {

    pub fn run(&self) -> () {

    }
}

impl FromStr for Instruction {
    type Err = ParseIntError;
  
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        return Ok(Instruction::Nop);
    }
}

impl FromStr for Program {
    type Err = ParseIntError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        return Ok( Program { instructions: vec![], 
                             accumulator: 0,
                             next: 0});
    }
}

pub fn read_input(filename: &str) -> Program {
    let input: String = crate::helpers::read_string((&filename).to_string());
    let program: Program = input.parse().unwrap();
    return program;
}

pub fn solve() {
    let filename = "a08_handheld_halting/input.txt";
    let program = read_input(filename);
    let result1 = -1;
    let result2 = -1;
    println!("08 - handheld halting: {} {}", result1, result2);
}

#[cfg(test)]
mod tests {
    use crate::a08_handheld_halting::*;

    fn read_test_input() -> Program {
        let filename = "src/a08_handheld_halting/test_input.txt";
        let program = read_input(filename);
        return program;
    }

    #[test]
    fn running_test_program_should_return_5() {
        let program = read_test_input();
        program.run();
        assert_eq!(5, program.accumulator);
    }    
}



