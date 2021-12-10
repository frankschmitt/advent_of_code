use std::collections::HashMap;

pub fn solve() {
    let filename = "a10_syntax_scoring/input.txt";
    //let filename = "a10_syntax_scoring/example_input.txt";
    let v = crate::helpers::read_string_list((&filename).to_string());

    let points = HashMap::from([ 
      (')', 3),
      (']', 57),
      ('}', 1197),
      ('>', 25137)
    ]);

    let opening_chars = vec!['(', '[', '{', '<' ];
    let matching_pairs = vec![('(', ')'), ('[', ']'), ('{', '}'), ('<', '>') ];
    let mut result1 = 0;
    for line in v{
        let mut stack = vec![];
        for ch in line.chars() {
            if opening_chars.contains(&ch) {
                stack.push(ch);
            }
            else {
                let ch2 = stack.pop().unwrap();
                if !matching_pairs.contains(&(ch2, ch)) {
                    result1 += points[&ch];
                    println!("found error for {}, mismatch: {}", line, ch);
                    break;
                }
            }
        }
    }
    //let result1 = -1;
    let result2 = -1;
    println!("10 - syntax scoring: {} {}", result1, result2);
}