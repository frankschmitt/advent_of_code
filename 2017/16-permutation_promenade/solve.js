var fs = require("fs");

// -- BORING STUFF (helpers I wrote because I was offline at the time) --- 

// custom conversion to string for printing Javascript objects
object_to_string = function(o) {
  var s = "{ ";
  for(var p in o) {
    s += p + ": " + o[p] + ", "; 
  }
  s += "}";
  return s;
}

// custom conversion to string for printing Javascript values
to_string = function(o) {
  var result = "";
  switch(typeof o) {
    case "object": result = object_to_string(o);
                   break;
    default: result = o;
  }
  return result;
}

// equality assertion
assert_equals = function(expected, got, msg) {
  var e = to_string(expected);
  var g = to_string(got); 
  if (e == g) {
    console.log(".");
    return 1;
  }
  else {
    console.log("\nERROR in " + msg + " : expected " + e + ", got " + g + "\n");
    return 0;
  }
}

// -- INTERESTING STUFF STARTS HERE --

/* parse a command string
 *  @returns record of command type and command argument
 * */
parse_command = function(str) {
  var type = str.charAt(0);
  var rest = str.substring(1, str.length);
  var arg1 = undefined;
  var arg2 = undefined;
  switch(type) {
    case "s":
      arg1 = parseInt(rest);
      break;
    case "x":
      a = rest.split("/"); 
      arg1 = parseInt(a[0]);
      arg2 = parseInt(a[1]);
    case "p":
      a = rest.split("/"); 
      arg1 = a[0];
      arg2 = a[1];
      break; 
  }
  return { type: type, arg1: arg1, arg2: arg2 };
}

// perform a single step (given by a command)
step = function(formation, cmd_str) {
  var cmd = parse_command(cmd_str);
  switch(cmd.type) {
    case "s": 
      return formation.slice(-cmd.arg1).concat(formation.slice(0, formation.length - cmd.arg1));
    case "x":
      var tmp = formation[cmd.arg1];
      formation[cmd.arg1] = formation[cmd.arg2];
      formation[cmd.arg2] = tmp;
      return formation;
    case "p":
      return formation.map(function(x) { 
        if (x == cmd.arg1) { return cmd.arg2 }
        else if (x == cmd.arg2) { return cmd.arg1 }
        else { return x }
      });
  }
}

// read an input file, and return the list of command strings
read_input_file = function(cmd_file) {
  var text = fs.readFileSync(cmd_file, "utf8");
  var re = /[^a-z0-9/,]/; // we want only letters, numbers, / and ,
  return text.replace(re, "").split(",");
}

// solve a puzzle, given by the input and the file name containing the commands
solve = function(formation, cmd_file, num_iterations) {
  var commands = read_input_file(cmd_file);
  var current = formation;
  num_iterations = num_iterations || 1; // default: one iteration
  // TODO: detect cycles, and short-circuit the evaluation accordingly
  for (var i=0; i < num_iterations; i++) {
    // TODO: use fold / reduce instead of forEach
    commands.forEach(function(command) {
      current = step(current, command);
    });
  }
  return current; 
}

// tests for parsing commands
test_parse_command = function() {
  assert_equals({ type: "s", arg1: 1, arg2: undefined }, parse_command("s1"), "s1");
  assert_equals({ type: "x", arg1: 3, arg2: 4 }, parse_command("x3/4"), "x3/4");
  assert_equals({ type: "p", arg1: "e", arg2: "b" }, parse_command("pe/b"), "pe/b");
}

// tests for reading input file
test_read_input_file = function() {
  assert_equals([ "s1", "x3/4", "pe/b" ],
      read_input_file("sample_input.txt"), 
      "parsing sample input");
}

// tests for the single-step function
test_step = function () {
  // spin
  assert_equals(['e','a','b','c','d'], step(['a','b','c','d','e'], "s1"), "s1");
  assert_equals(['c','d','e','a','b'], step(['a','b','c','d','e'], "s3"), "s3"); 
  // swap by index
  assert_equals(['e','a','b','d','c'], step(['e','a','b','c','d'], "x3/4"), "x3/4");
  // swap by name
  assert_equals(['b','a','e','d','c'], step(['e','a','b','d','c'], "pe/b"), "pe/b");
}

test_solve = function() {
  assert_equals(['b','a','e','d','c'], solve(['a','b','c','d','e'], "sample_input.txt"), "sample input"); 
}

test_parse_command();
test_read_input_file();
test_step();
test_solve();
console.log("part I: " + 
    solve(['a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p'], 
      "input.txt").join(""));
console.log("part I): " + 
    solve(['a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p'], 
      "input.txt", 300).join(""));
