use regex::Regex;

// day 2 is pretty straightforward: parse each line, count the number of matching letters in
// the substring (part 1) or at positions min/max (day2), and increment the counter if the hit
// count equals 1
pub fn solve() {
    let filename = "a02_password_philosophy/input.txt";
    let values = crate::helpers::read_string_list((&filename).to_string());
    // println!("got values: {}", values.len());
    // a line of input consists of: <min>-<max> <letter>: <password>
    let re = Regex::new(r"(\d+)-(\d+) ([a-z]): ([a-z]+)").unwrap();
    /* let re = Regex::new(r"(?x)
       (?P<min>\d+)  # min number of occurrences
       -
       (?P<max>\d+) # max number of occurrences

       (?P<letter>[a-z])   # the letter
       :

       (?P<password>[a-z]+) # the password
       ").unwrap(); */
    let mut result1: usize = 0;
    let mut result2: usize = 0;
    for line in values {
        // println!("parsing line {}", &line);
        let md = re.captures(&line).unwrap();
        // println!("found entry, min = {}, max = {}, letter = {}, password = {}", &md["min"], &md["max"], &md["letter"], &md["password"]);
        let min: usize = md[1].parse().unwrap();
        let max: usize = md[2].parse().unwrap();
        let ch: char = md[3].chars().nth(0).unwrap();
        let password: String = md[4].to_string();
        // println!("found entry, min = {}, max = {}, letter = {}, password = {}", &md[1], &md[2], &md[3], &md[4]);
        // part 1: count the number of occurrences in the password overall
        let hit_count1 = password.chars().filter(|x| x == &ch).count();
        // part 2: count the number of occurrences in the slice from min to max (1-based); must
        // be exactly 1!
        let hit_count2 = password[min-1 .. min].chars().filter(|x| x == &ch).count() + 
            password[max-1 .. max].chars().filter(|x| x == &ch).count();
        // println!("found entry, min = {}, max = {}, letter = {}, password = {}, hit_count1 = {}, hit_count2 = {}", &min, &max, &ch, &password, &hit_count1, &hit_count2);
        if (min <= hit_count1) && (hit_count1 <= max) {
            result1 += 1;
        }
        if hit_count2 == 1 {
            result2 += 1;
        }
    }
    println!("02 - password philosophy : {} {}", result1, result2);
}
