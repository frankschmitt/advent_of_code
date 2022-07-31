pub fn solve() {
    let filename = "a17_trick_shot/example_input.txt";
    //let filename = "a17_trick_shot/input.txt";
    let input = crate::helpers::read_string_list((&filename).to_string());
    let start_pos1: u32 = parse_pos(&input[0]);
    let result1 = -1;
    let result2 = -1;
    println!("17 - trick shot: {} {}", result1, result2);
}