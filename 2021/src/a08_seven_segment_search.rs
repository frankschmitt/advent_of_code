#[derive(Debug, Eq, PartialEq)]
pub struct Entry {
    signal_patterns: Vec<String>,
    output_values: Vec<String>
}

pub fn parse_entry(s: &str) -> Entry {
    let (s1, s2) = s.split_once('|').unwrap();
    let signal_patterns: Vec<String> = s1.split_terminator(' ').map(|s| s.to_string()).collect();
    let output_values: Vec<String> = s2.split_terminator(' ').map(|s| s.to_string()).collect();
    return Entry { signal_patterns, output_values };
}

pub fn solve() {
    let filename = "a08_seven_segment_search/input.txt";
    //let filename = "a08_seven_segment_search/example_input.txt";
    let v = crate::helpers::read_string_list((&filename).to_string());
    let entries: Vec<Entry> = v.iter().map(|line| parse_entry(line)).collect();
    // part I: simply call the output values corresponding to 1 (two segments), 4 (four segments), 7 (three segments) and 8 (seven segments)
    let mut matching_words = vec![];
    for e in entries {
        let mut its_matching_words: Vec<String> = e.output_values.iter().filter(|w| vec![2,4,3,7].contains(&w.len())).map(|s| s.to_string()).collect();
        matching_words.append(&mut its_matching_words);
    }
    // println!("matching words: {:?}", matching_words);
    //let result1 = matching_word_lengths.fold(0, |acc, x| acc + x);
    let result1 = matching_words.len();
    let result2 = -1;
    println!("08 seven segment search: {} {}", result1, result2);
}