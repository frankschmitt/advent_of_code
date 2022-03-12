use std::cmp::{ min, max };
use itertools::Itertools;
use std::collections::HashMap;

fn parse_pos(line: &str) -> u32 {
    let (_, pos_str) = line.split_once(": ").unwrap();
    return pos_str.parse::<u32>().unwrap();
}

// move n steps, and wrap around to position 1 after 10
fn compute_new_pos(old_pos: u32, n: u32) -> u32 {
  return (old_pos - 1 + n) % 10 + 1;
}

// roll the die and move
//   @returns new die, die rolls and pos
fn roll_and_move(player_name: &str, die: u32, die_rolls: u32, pos: u32) -> (u32, u32, u32) {
  let new_die1 = (die - 1 + 1) % 100 + 1;
  let new_die2 = (new_die1 - 1 + 1) % 100 + 1;
  let new_die3 = (new_die2 - 1 + 1) % 100 + 1;
  let new_pos = compute_new_pos(pos, new_die1 + new_die2 + new_die3);
  // println!("player {} rolls {}+{}+{} and moves to {}", player_name, new_die1, new_die2, new_die3, new_pos);
  return (new_die3, die_rolls + 3, new_pos);
}

pub fn solve1(start_pos1: u32, start_pos2: u32) -> u32 {
    let mut pos1 = start_pos1;
    let mut pos2 = start_pos2;
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
    println!("score1: {}, score2: {}, die_rolls: {}", score1, score2, die_rolls);
    return min(score1, score2) * die_rolls;
}

#[derive(PartialEq, Eq, Hash, Debug)]
pub struct BoardState {
    scores: Vec<u32>,
    positions: Vec<u32>,
    next_player: usize,
    winner: usize // 0: none, 1: player1, 2: player2
}

#[derive(Debug)]
pub struct MultiverseState {
  // board states plus the number of paths leading to them
  board_states: HashMap<BoardState, u128>
}

impl MultiverseState {
    pub fn init(start_state: BoardState) -> MultiverseState {
        let mut start_states = HashMap::new();
        start_states.insert(start_state, 1); // only 1 path leads to the start state
        return MultiverseState { board_states: start_states}
    }

    // run until all states in the multiverse state have terminated (a winner)
    pub fn run(state: MultiverseState, winning_score: u32) -> MultiverseState {
        let mut terminated = true;
        let mut new_board_states: HashMap<BoardState, u128> = HashMap::new();
        for bs in state.board_states {
            // board already has a winner? copy it over unchanged
            if bs.0.winner != 0 {
                *new_board_states.entry(bs.0).or_insert(0) += bs.1;
            }
            else {
                terminated = false;
                // for each of the 27 possible combinations: compute the successor state
                for die1 in [1, 2, 3] {
                  for die2 in [1, 2, 3] {
                      for die3 in [1, 2, 3] {
                        let new_pos = compute_new_pos(bs.0.positions[bs.0.next_player], die1+die2+die3);
                        let new_positions = match bs.0.next_player {
                            1 => vec![ 0, new_pos, bs.0.positions[2]],
                            2 => vec![ 0, bs.0.positions[1], new_pos],
                            _ => panic!("unexpected next player")
                        };
                        let new_score = bs.0.scores[bs.0.next_player] + new_pos;
                        let new_scores = match bs.0.next_player {
                            1 => vec![0, new_score, bs.0.scores[2]],
                            2 => vec![0, bs.0.scores[1], new_score],
                            _ => panic!("unexpected next player")
                        };
                        let new_winner = if new_score >= winning_score { bs.0.next_player } else { 0 };
                        let new_next_player = if bs.0.next_player == 1 { 2 } else { 1 };
                        let board_state = BoardState { scores: new_scores, positions: new_positions, next_player: new_next_player,
                          winner: new_winner };
                        *new_board_states.entry(board_state).or_insert(0) += bs.1; // add path
                      }
                  }
                }
            }
        }   
        let next_state = MultiverseState { board_states: new_board_states };
        return match terminated {
            false => MultiverseState::run(next_state, winning_score),
            true => next_state
        };
    }
}


pub fn solve2(start_pos1: u32, start_pos2: u32, winning_score: u32) -> u128 {
    let board_state = BoardState { scores: vec![0, 0, 0], positions: vec![0, start_pos1, start_pos2], next_player: 1, winner: 0 };
    let multiverse_state = MultiverseState::init(board_state);
    let final_state = MultiverseState::run(multiverse_state, winning_score);
    /*
      idea for part II:
        - the current state can be modeled by (score1, pos1, score2, pos2, next_player); there's no die state to keep track of
        - use equivalence classes to model the current state
        - we also need to keep track of the number of paths that lead us to this state
        - data model:
            dictionary, key: state (score1, pos1, score2, pos2, next_player), value: number of paths
        - in each step
            new_state = {}
            for each entry in old_state:
              if a player has already won:
                 add the entry unchanged to new_state 
              for each die_roll in ((1,1,1), (1,1,2), (1,1,3), ..., (3,3,3)):
                 new_pos += die_roll
                 new_score += new_pos
                 update new_state for this key; if it doesn't exit: add new entry, number of paths = number of paths for current state
                                                 otherwise: number of paths += number of paths for current state
            exit if all states have finished (i.e. no new states were added)
    */
  let mut wins: HashMap<usize, u128> = HashMap::new();
  for bs in final_state.board_states {
    *wins.entry(bs.0.winner).or_insert(0) += bs.1;
  } 
  return max(wins[&1], wins[&2]);
}

pub fn solve() {
    //let filename = "a21_dirac_dice/example_input.txt";
    let filename = "a21_dirac_dice/input.txt";
    let input = crate::helpers::read_string_list((&filename).to_string());
    let start_pos1: u32 = parse_pos(&input[0]);
    let start_pos2: u32 = parse_pos(&input[1]);
    let result1 = solve1(start_pos1, start_pos2);
    let result2 = solve2(start_pos1, start_pos2, 21);
    println!("21 - dirac dice: {} {}", result1, result2);
}