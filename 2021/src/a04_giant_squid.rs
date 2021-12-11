use ndarray::*;

#[derive(Debug, Eq, PartialEq)]
pub struct Board {
    numbers: Array2<u64>,
    marked: Array2<u8>
}

#[derive(Debug, Eq, PartialEq)]
pub struct Bingo {
    numbers: Vec<u64>,
    boards: Vec<Board>
}

pub fn parse_boards(input: &[String]) -> Vec<Board> {
    let mut result = vec![];
    // each boards is 5x5 followed by an empty line - let's chunk it!
    for chk in input.chunks(6) {
        let mut number_vec = vec![];
        for i in 0 .. 5 {
            println!("parsing chunk: {}", chk[i]);
            let mut parts: Vec<u64> = chk[i].split_whitespace().map(|val| val.parse::<u64>().unwrap()).collect();
            number_vec.append(&mut parts);
        }
        let numbers = Array::from_shape_vec((5, 5), number_vec).unwrap();
        let marked = Array::zeros((5,5));
        result.push(Board { numbers: numbers, marked: marked});
    }
    return result;
}

pub fn parse_bingo(input: &Vec<String>) -> Bingo {
    let line = input.first().unwrap();
    let numbers: Vec<u64> = line.split(',').map(|val| val.parse::<u64>().unwrap()).collect();
    let boards = parse_boards(&input[2..]);
    return Bingo { numbers: numbers, boards: boards};
}

pub fn solve() {
    // let filename = "a04_giant_squid/input.txt";
    let filename = "a04_giant_squid/example_input.txt";
    let v = crate::helpers::read_string_list((&filename).to_string());   
    let bingo = parse_bingo(&v);
    println!("bingo: {:?}", bingo);

    let result1 = -1;
    let result2 = -1;
    println!("04 - giant squid: {} {}", result1, result2);
}