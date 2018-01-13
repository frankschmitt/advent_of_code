// some helpers (without Internet connection, you have to build some very basic things yourself ;-) )

object_to_string = function(o) {
  var s = "{ ";
  for(var p in o) {
    s += p + ": " + o[p] + ", "; 
  }
  s += "}";
  return s;
}

// custom conversion to string for printing Javascript objects
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

/*
   Spin, written sX, makes X programs move from the end to the front, but maintain their order otherwise. (For example, s3 on abcde produces cdeab).
   Exchange, written xA/B, makes the programs at positions A and B swap places.
   Partner, written pA/B, makes the programs named A and B swap places.
 * */

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

// basic steps
step = function(formation, cmd_str) {
  var cmd = parse_command(cmd_str);
  switch(cmd.type) {
    case "s": 
      //console.log("slicing " + formation + " with " + cmd.type + ", " + cmd.arg);
      //console.log("result: " + formation.slice(-cmd.arg) + " + " + formation.slice(0, formation.length - cmd.arg));
      return formation.slice(-cmd.arg1) + formation.slice(0, formation.length - cmd.arg1);
      break;
  }
}

test_parse_command = function() {
  assert_equals({ type: "s", arg1: 1, arg2: undefined }, parse_command("s1"), "s1");
  assert_equals({ type: "x", arg1: 3, arg2: 4 }, parse_command("x3/4"), "x3/4");
  assert_equals({ type: "p", arg1: "e", arg2: "b" }, parse_command("pe/b"), "pe/b");
}

/*
   For example, with only five programs standing in a line (abcde), they could do the following dance:

   s1, a spin of size 1: eabcd.
   x3/4, swapping the last two programs: eabdc.
   pe/b, swapping programs e and b: baedc.
 * */
test_step = function () {
  // spin
  assert_equals("eabcd", step("abcde", "s1"), "s1");
  assert_equals("cdeab", step("abcde", "s3"), "s3"); 
  // swap by index
  assert_equals("eabdc", step("eabcd", "x3/4"), "x3/4");
}

test_parse_command();
test_step();

