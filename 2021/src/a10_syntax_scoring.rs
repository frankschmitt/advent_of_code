use std::collections::HashMap;

pub fn solve() {
    let filename = "a10_syntax_scoring/input.txt";
    //let filename = "a10_syntax_scoring/example_input.txt";
    let v = crate::helpers::read_string_list((&filename).to_string());

    // points for error
    let error_points = HashMap::from([ 
      (')', 3),
      (']', 57),
      ('}', 1197),
      ('>', 25137)
    ]);

    // points for line repairs
    // we use the opening delimiters so we can directly look up the point from the 
    let repair_points = HashMap::from([ 
        ('(', 1),
        ('[', 2),
        ('{', 3),
        ('<', 4)
      ]);
    
    let opening_chars = vec!['(', '[', '{', '<' ];
    let matching_pairs = vec![('(', ')'), ('[', ']'), ('{', '}'), ('<', '>') ];
    let mut result1 = 0;
    let mut repair_scores = vec![];
    for line in v{
        let mut stack = vec![];
        let mut error = false;
        for ch in line.chars() {
            if opening_chars.contains(&ch) {
                stack.push(ch);
            }
            else {
                let ch2 = stack.pop().unwrap();
                if !matching_pairs.contains(&(ch2, ch)) {
                    result1 += error_points[&ch];
                    // println!("found error for {}, mismatch: {}", line, ch);
                    error = true;
                    break;
                }
            }
        }
        if !error {
            let mut part_result2: usize = 0;
            loop {
                if stack.is_empty() { break; }
                let ch = stack.pop().unwrap();
                part_result2 *= 5;
                part_result2 += repair_points[&ch];
            }
            // println!("repair score for {} : {}", line, part_result2);
            repair_scores.push(part_result2);
        }
    }
    repair_scores.sort();
    // println!("repair scores: {:?}", repair_scores);
    let result2 = repair_scores[repair_scores.len() / 2];
    println!("10 - syntax scoring: {} {}", result1, result2);
}