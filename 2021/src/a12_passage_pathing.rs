use petgraph::graphmap::*;

// walk all paths from start to end (ignoring all small caves that have already been visited)
//   strangely, there seems to be no standard implementation for this - we have to roll our own
fn walk_small_only_once(g: &UnGraphMap<&str, i32>, start: &str, end: &str, visited: &Vec<&str>, found_paths: &Vec<String>) -> Vec<String> {
    // println!("visiting {}, found_paths: {:?}", start, found_paths);
    if start == end {
        return found_paths.to_vec();
    }
    let mut result = vec![];
    for n in g.neighbors(start) {
       // only visit caves that either haven't been visited so far or that are large (uppercase)
       if !visited.contains(&n) || n.chars().nth(0).unwrap().is_ascii_uppercase() {
         let mut my_visited = visited.clone();  
         my_visited.push(n);
         let mut my_found_paths = found_paths.clone();
         for p in my_found_paths.iter_mut() {
             p.push_str(n);
         } 
         let mut its_paths = walk_small_only_once(g, n, end, &my_visited, &my_found_paths);
         result.append(&mut its_paths); 
      }
    }
    return result;
}

fn find_all_paths_small_only_once(g: &UnGraphMap<&str, i32>, start: &str, end: &str) -> Vec<String> {
    return walk_small_only_once(g, start, end, &vec![&"start".to_string()], &vec!["start".to_string()]);
}

// walk all paths from start to end (allowing a single small cave to be visited twice)
//   strangely, there seems to be no standard implementation for this - we have to roll our own
fn walk_single_small_twice(g: &UnGraphMap<&str, i32>, start: &str, end: &str, visited: &Vec<&str>, found_paths: &Vec<String>, visited_twice: bool) -> Vec<String> {
    // println!("visiting {}, found_paths: {:?}", start, found_paths);
    if start == end {
        return found_paths.to_vec();
    }
    let mut result = vec![];
    for n in g.neighbors(start) {
       if n == "start" { continue; }
       let is_large_cave = n.chars().nth(0).unwrap().is_ascii_uppercase();
       // only visit caves that are large (uppercase) OR small caves if we didn't visit a single small cave twice before
       if !visited_twice || is_large_cave || !visited.contains(&n) {
         // second visit to small cave? remember this!
         let my_visited_twice = visited_twice || (!is_large_cave && visited.contains(&n));
         let mut my_visited = visited.clone();  
         my_visited.push(n);
         let mut my_found_paths = found_paths.clone();
         for p in my_found_paths.iter_mut() {
             p.push_str(n);
         } 
         let mut its_paths = walk_single_small_twice(g, n, end, &my_visited, &my_found_paths, my_visited_twice);
         result.append(&mut its_paths); 
      }
    }
    return result;
}

fn find_all_paths_single_small_twice(g: &UnGraphMap<&str, i32>, start: &str, end: &str) -> Vec<String> {
    return walk_single_small_twice(g, start, end, &vec![&"start".to_string()], &vec!["start".to_string()], false);
}

pub fn solve() {
    //let filename = "a12_passage_pathing/example_input_small.txt";
    //let filename = "a12_passage_pathing/example_input.txt";
    //let filename = "a12_passage_pathing/example_input_large.txt";
    let filename = "a12_passage_pathing/input.txt";
    let v = crate::helpers::read_string_list((&filename).to_string());   
    // build graph from edge list
    let edges: Vec<(&str, &str)> = v.iter().map(|line| line.split_once('-').unwrap()).collect();
    let mut g = UnGraphMap::new();
    for e in edges {
        g.add_edge(e.0, e.1, -1);
    }
    // println!("graph: {:?}", g);
    
    // part 1: only walk small caves once
    let paths1 = find_all_paths_small_only_once(&g, "start", "end");
    // println!("paths1: {:?}", paths1);
    let result1 = paths1.len();
    
    // part 2: allow walking a single small cave twice
    let paths2 = find_all_paths_single_small_twice(&g, "start", "end");
    // println!("paths2: {:?}", paths2);
    let result2 = paths2.len();
    
    println!("12 - passage pathing: {} {}", result1, result2);
}