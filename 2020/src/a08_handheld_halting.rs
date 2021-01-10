use core::str::FromStr;
use regex::Regex;
use std::convert::TryInto;
use std::clone::Clone;

#[derive(Debug, PartialEq, Clone)]
pub enum Instruction {
    Acc(i64),
    Jmp(i64),
    Nop(i64)
}

#[derive(Debug, PartialEq, Clone)]
pub enum ProgramResult {
    InfiniteLoop,
    Terminated
}

#[derive(Debug, PartialEq, Clone)]
pub enum ParseProgramError {
    ParseInstructionError
}

#[derive(Debug, PartialEq, Clone)]
pub struct Program {
    instructions: Vec<Instruction>,
    accumulator: i64, 
    next: usize, // index of the next instruction to execute
    execution_counts: Vec<usize>
}

use crate::a08_handheld_halting::ProgramResult::*;
   
impl Program {
    
    pub fn run(&mut self) -> ProgramResult {
        if self.next >= self.instructions.len() {
            return Terminated;
        }
        let old_count = self.execution_counts[self.next];
        if old_count > 0 {
            return InfiniteLoop;
        }
        else {
            self.execution_counts[self.next] += 1;
            let instruction = &self.instructions[self.next];
            // increase accu only for Acc instruction
            self.accumulator = match instruction {
                Instruction::Acc(i) => self.accumulator + i,
                _ => self.accumulator
            };
            // set next depending on instruction type
            self.next = match instruction {
                Instruction::Jmp(i) => ((self.next as i64) + i).try_into().unwrap(),
                _ => self.next + 1
            };
            return self.run(); // recursive call
        }
    }
}

impl FromStr for Instruction {
    type Err = ParseProgramError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let re = Regex::new(r"^(acc|jmp|nop) ([+-]\d+)$").unwrap();
        let result:Result<Self, Self::Err> = match re.captures(s) {
            None => Err(ParseProgramError::ParseInstructionError),
            // md[0] is the complete match - the captured groups start at 1
            Some(md) => Ok(match &md[1] {
                "acc" => Instruction::Acc(md[2].parse::<i64>().unwrap()),
                "jmp" => Instruction::Jmp(md[2].parse::<i64>().unwrap()),
                _ => Instruction::Nop(md[2].parse::<i64>().unwrap())
            }) 
        };
        // println!("parsed '{}' -> {}", s, crate::helpers::result_to_string(&result));
        return result;
    }
}

impl FromStr for Program {
    type Err = ParseProgramError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let instructions: Vec<Instruction> = s.split("\n")
                                              .filter(|s| s.len() > 0)
                                              .map(|line| line.parse().unwrap())
                                              .collect::<Vec<_>>();
        let execution_counts: Vec<usize> = vec![0; instructions.len()];
        return Ok( Program { instructions: instructions, 
                             accumulator: 0,
                             next: 0,
                             execution_counts: execution_counts});
    }
}

pub fn read_input(filename: &str) -> Program {
    let input: String = crate::helpers::read_string((&filename).to_string());
    let program: Program = input.parse().unwrap();
    return program;
}

// run the permutations, and return the single one that terminates normally
pub fn run_permutations(master: &Program) -> Option<Program> {
    for i in 0 .. master.instructions.len() {
        let instruction = &master.instructions[i];
        match instruction {
            Instruction::Acc(_) => {},
            Instruction::Jmp(x) => {
                let mut copy = master.clone();
                copy.instructions[i] = Instruction::Nop(*x);
                match copy.run() {
                    InfiniteLoop => (),
                    Terminated => return Some(copy)
                }
            },
            Instruction::Nop(x) => {
                let mut copy = master.clone();
                copy.instructions[i] = Instruction::Jmp(*x);
                match copy.run() {
                    InfiniteLoop => (),
                    Terminated => return Some(copy)
                }
            },
        } 
    }
    return None;
}

pub fn solve() {
    let filename = "a08_handheld_halting/input.txt";
    let master = read_input(filename);
    let mut program = master.clone();
    program.run();
    let result1 = program.accumulator;
    let result2 = run_permutations(&master).unwrap().accumulator;
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
    fn parsing_a_single_instruction() {
        assert_eq!(Ok(Instruction::Nop(0)), "nop +0".parse());
        assert_eq!(Ok(Instruction::Jmp(-4)), "jmp -4".parse());
        assert_eq!(Ok(Instruction::Acc(99)), "acc +99".parse());
    }

    #[test]
    fn parsing_test_program_should_give_eleven_instructions() {
        let program = read_test_input();
        assert_eq!(9, program.instructions.len());
    }

    #[test]
    fn running_test_program_should_return_5() {
        let mut program = read_test_input();
        program.run();
        assert_eq!(5, program.accumulator);
    }    

    #[test]
    fn running_fixed_test_program_should_return_8() {
        let master = read_test_input();
        let copy = run_permutations(&master).unwrap();
        assert_eq!(8, copy.accumulator);
    
    }
}



