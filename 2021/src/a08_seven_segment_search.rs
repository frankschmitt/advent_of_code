use std::collections::{HashMap, BTreeSet, HashSet};

#[derive(Debug, Eq, PartialEq)]
pub struct Entry {
    signal_patterns: Vec<String>,
    output_values: Vec<String>
}

// parse an entry 
pub fn parse_entry(s: &str) -> Entry {
    let (s1, s2) = s.split_once('|').unwrap();
    let signal_patterns: Vec<String> = s1.split_terminator(' ').map(|s| s.to_string()).collect();
    let output_values: Vec<String> = s2.trim_start_matches(' ').split_terminator(' ').map(|s| s.to_string()).collect();
    return Entry { signal_patterns, output_values };
}

// check whether s1 is a subset of s2 
fn is_subset_of(s1: &BTreeSet<char>, s2: &BTreeSet<char>) -> bool {
    return s1.iter().all(|item| s2.contains(item));
}

// find the missing value (= segment)
fn find_missing_value(hm: &HashMap<char, char>) -> char {
    let all_vals: HashSet<_> = ['a', 'b', 'c', 'd', 'e', 'f', 'g'].iter().cloned().collect();
    let its_vals: HashSet<_> = hm.values().cloned().collect();
    // Can be seen as `a - b`.
    for val in all_vals.difference(&its_vals) {
        return *val;
    }
    // dummy return to satisfy compiler (I don't want to return an Option<> here)
    return 'x';
}

pub fn solve() {
    let filename = "a08_seven_segment_search/input.txt";
    //let filename = "a08_seven_segment_search/example_input.txt";
    let v = crate::helpers::read_string_list((&filename).to_string());
    //let v = vec!["acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"];
    let entries: Vec<Entry> = v.iter().map(|line| parse_entry(line)).collect();
    // part I: simply count the output values corresponding to 1 (two segments), 4 (four segments), 7 (three segments) and 8 (seven segments)
    let mut matching_words = vec![];
    for e in entries.iter() {
        let mut its_matching_words: Vec<String> = e.output_values.iter().filter(|w| vec![2,4,3,7].contains(&w.len())).map(|s| s.to_string()).collect();
        matching_words.append(&mut its_matching_words);
    }
    // println!("matching words: {:?}", matching_words);
    let result1 = matching_words.len();

    // part II is tricky; we use the following set of rules to determine which input string belongs to which input digit
    //    length 2 => 1
    //    length 3 => 4
    //    length 4 => 7
    //    length 7 => 8
    //    length 6 and doesn't contain input for 1 => 6
    //    length 6 and contains input for 4 => 9
    //    length 6 and contains input for 1, but not for 4 => 0
    //    length 5 and contains input for 1 => 3
    //    length 5 and is contained in input for 6 => 5
    //    length 5 and is not contained in input for 6 => 2
    //  from that, we can determine the segments; here's the correct wiring
    //    
    //     aaaa 
    //    b    c
    //    b    c
    //     dddd
    //    e    f
    //    e    f
    //     gggg
    //
    //    segment a is the one that is set in 7 but not in 1
    //    segment b is the one that is set in 9 but not in 3
    //    segment c is the one that is not set in 6
    //    segment d is the one that is not set in 0
    //    segment e is the one that is not set in 9
    //    segment f is the one that is set in 1 but not in 2
    //    segment g is the one that is set for 0, 2, 3, 5, 6, 8, 9, but not for 1, 4, 7 (or simply the one that remains)
    let mut result2 = 0;
    for e in entries.iter() {
        let mut input_patterns = HashMap::<u8, BTreeSet::<char>>::new();
        // 1, 4, 7 and 8 are straightforward because they have unique lengths
        input_patterns.insert(1, e.signal_patterns.iter().filter(|p| p.len() == 2).nth(0).unwrap().chars().collect());       
        input_patterns.insert(4, e.signal_patterns.iter().filter(|p| p.len() == 4).nth(0).unwrap().chars().collect());       
        input_patterns.insert(7, e.signal_patterns.iter().filter(|p| p.len() == 3).nth(0).unwrap().chars().collect());       
        input_patterns.insert(8, e.signal_patterns.iter().filter(|p| p.len() == 7).nth(0).unwrap().chars().collect());       
        // 6 is the string with length 6 that doesn't contain the pattern for 1
        input_patterns.insert(6, e.signal_patterns.iter().filter(|p| {
            let its_chars: BTreeSet::<char> = p.chars().collect(); 
            return p.len() == 6 && !is_subset_of(&input_patterns[&1], &its_chars);
         }
        ).nth(0).unwrap().chars().collect());
        // 9 is the string with length 6 that contains the pattern for 4
        input_patterns.insert(9, e.signal_patterns.iter().filter(|p| {
            let its_chars: BTreeSet::<char> = p.chars().collect(); 
            return p.len() == 6 && is_subset_of(&input_patterns[&4], &its_chars);
         }
        ).nth(0).unwrap().chars().collect());
        // 0 is the string with length 6 that contains the pattern for 1 but doesn' contain the pattern for 4
        input_patterns.insert(0, e.signal_patterns.iter().filter(|p| {
            let its_chars: BTreeSet::<char> = p.chars().collect(); 
            return p.len() == 6 && is_subset_of(&input_patterns[&1], &its_chars) && !is_subset_of(&input_patterns[&4], &its_chars);
         }
        ).nth(0).unwrap().chars().collect());
        // 3 is the string with length 5 that contains the pattern for 1
        input_patterns.insert(3, e.signal_patterns.iter().filter(|p| {
            let its_chars: BTreeSet::<char> = p.chars().collect(); 
            return p.len() == 5 && is_subset_of(&input_patterns[&1], &its_chars);
         }
        ).nth(0).unwrap().chars().collect());
        // 5 is the string with length 5 that is contained in the pattern for 6
        input_patterns.insert(5, e.signal_patterns.iter().filter(|p| {
            let its_chars: BTreeSet::<char> = p.chars().collect(); 
            return p.len() == 5 && is_subset_of(&its_chars, &input_patterns[&6]);
         }
        ).nth(0).unwrap().chars().collect());
        // 2 is the string with length 5 that is neither contained in the pattern for 6 nor contained in the pattern for 9
        input_patterns.insert(2, e.signal_patterns.iter().filter(|p| {
            let its_chars: BTreeSet::<char> = p.chars().collect(); 
            return p.len() == 5 && !is_subset_of(&its_chars, &input_patterns[&6]) && !is_subset_of(&its_chars, &input_patterns[&9]);
         }
        ).nth(0).unwrap().chars().collect());
        // println!("input patterns for {:?} : \n{:?}\n", e.signal_patterns, input_patterns);

        // now we know which input pattern belongs to which input digit. From that, let's determine which segment is which
        let mut segments = HashMap::<char, char>::new();
        // segment a is the one that is set in 7 but not in 1
        segments.insert('a', *input_patterns[&7].iter().filter(|ch| !input_patterns[&1].contains(ch)).nth(0).unwrap());
        // segment b is the one that is set in 9 but not in 3
        segments.insert('b', *input_patterns[&9].iter().filter(|ch| !input_patterns[&3].contains(ch)).nth(0).unwrap());
        //    segment c is the one that is not set in 6
        segments.insert('c', *input_patterns[&8].iter().filter(|ch| !input_patterns[&6].contains(ch)).nth(0).unwrap());
        //    segment d is the one that is not set in 0
        segments.insert('d', *input_patterns[&8].iter().filter(|ch| !input_patterns[&0].contains(ch)).nth(0).unwrap());
        //    segment e is the one that is not set in 9
        segments.insert('e', *input_patterns[&8].iter().filter(|ch| !input_patterns[&9].contains(ch)).nth(0).unwrap());
        // segment f is the one that is set in 1 but not in 2
        segments.insert('f', *input_patterns[&1].iter().filter(|ch| !input_patterns[&2].contains(ch)).nth(0).unwrap());
        // segment g is the one that is set for 0, 2, 3, 5, 6, 8, 9, but not for 1, 4, 7 (or simply the one that remains unmapped)
        segments.insert('g', find_missing_value(&segments));
        // println!("segments for {:?} : \n{:?}\n", e, segments);

        // Progress! We can now determine the digits from the strings
        let digits_to_segments = HashMap::from([ 
            ("abcefg".chars().map(|ch| segments[&ch]).collect::<BTreeSet<char>>(), 0),
            ("cf".chars().map(|ch| segments[&ch]).collect::<BTreeSet<char>>(), 1),
            ("acdeg".chars().map(|ch| segments[&ch]).collect::<BTreeSet<char>>(), 2),
            ("acdfg".chars().map(|ch| segments[&ch]).collect::<BTreeSet<char>>(), 3),
            ("bcdf".chars().map(|ch| segments[&ch]).collect::<BTreeSet<char>>(), 4),
            ("abdfg".chars().map(|ch| segments[&ch]).collect::<BTreeSet<char>>(), 5),
            ("abdefg".chars().map(|ch| segments[&ch]).collect::<BTreeSet<char>>(), 6),
            ("acf".chars().map(|ch| segments[&ch]).collect::<BTreeSet<char>>(), 7),
            ("abcdefg".chars().map(|ch| segments[&ch]).collect::<BTreeSet<char>>(), 8),
            ("abcdfg".chars().map(|ch| segments[&ch]).collect::<BTreeSet<char>>(), 9),
        ]);
        // println!("digits_to_segments for {:?} : \n{:?}\n", e, digits_to_segments);

        // finally, we can map the output values to digits and compute the result for this entry 
        let mut partial_result2 = 0;
        for ov in e.output_values.iter() {
            // println!("digit: {}", ov);
            let its_chars = ov.chars().collect::<BTreeSet<char>>();
            let digit = digits_to_segments[&its_chars];
            partial_result2 = partial_result2*10 + digit;
        }
        // println!("partial result2 for {:?} : {}", e, partial_result2);
        result2 += partial_result2;
    }
   
    println!("08 seven segment search: {} {}", result1, result2);
}