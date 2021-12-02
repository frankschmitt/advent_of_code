pub fn solve() {
    let filename = "a02_dive/input.txt";
    //let filename = "a02_dive/example_input.txt";
    let a = crate::helpers::read_string_list((&filename).to_string());
    let mut horiz = 0;
    let mut depth1 = 0;
    let mut depth2 = 0;
    let mut aim = 0;
    for line in a {
        let parts = line.split(" ").collect::<Vec<&str>>();
        if parts.len() < 2 {
            panic!("unexpected input: {}", line);
        }
        let num = parts[1].parse::<i64>().unwrap();
        match parts[0] {
            "forward" => { 
                horiz += num;
                depth2 += num*aim;
            },
            "down" => {
                depth1 += num;
                aim += num;
            },
            "up" => {
                depth1 -= num;
                aim -= num;
            }, 
            _ => panic!("unexpected input: {}", line)
        }
    }
    let result1 = horiz * depth1;
    let result2 = horiz * depth2;
    println!("02 - dive! : {} {}", result1, result2);
}