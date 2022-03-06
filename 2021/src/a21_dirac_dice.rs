use std::cmp::min;

fn parse_pos(line: &str) -> u32 {
    let (_, pos_str) = line.split_once(": ").unwrap();
    return pos_str.parse::<u32>().unwrap();
}

// roll the die and move
//   @returns new die, die rolls and pos
fn roll_and_move(player_name: &str, die: u32, die_rolls: u32, pos: u32) -> (u32, u32, u32) {
  let new_die1 = (die - 1 + 1) % 100 + 1;
  let new_die2 = (new_die1 - 1 + 1) % 100 + 1;
  let new_die3 = (new_die2 - 1 + 1) % 100 + 1;
  let new_pos = (pos - 1 + new_die1 + new_die2 + new_die3) % 10 + 1;
  // println!("player {} rolls {}+{}+{} and moves to {}", player_name, new_die1, new_die2, new_die3, new_pos);
  return (new_die3, die_rolls + 3, new_pos);
}

pub fn solve() {
    //let filename = "a21_dirac_dice/example_input.txt";
    let filename = "a21_dirac_dice/input.txt";
    let input = crate::helpers::read_string_list((&filename).to_string());
    let mut pos1: u32 = parse_pos(&input[0]);
    let mut pos2: u32 = parse_pos(&input[1]);
    let mut die: u32 = 100; // start at 100, so the first die roll is a 1
    let mut die_rolls: u32 = 0;
    let mut die_rolls: u32 = 0;
    let mut score1: u32 = 0;
    let mut score2: u32 = 0;
    loop {
        (die, die_rolls, pos1) = roll_and_move("1", die, die_rolls, pos1);
        score1 += pos1;
        if score1 >= 1000 {
            break;
        } 
        (die, die_rolls, pos2) = roll_and_move("2", die, die_rolls, pos2);
        score2 += pos2;
        if score2 >= 1000 {
            break;
        }
    }
    let result1 = min(score1, score2) * die_rolls;
    let result2 = -1;
    println!("score1: {}, score2: {}, die_rolls: {}", score1, score2, die_rolls);
    println!("21 - dirac dice: {} {}", result1, result2);
}