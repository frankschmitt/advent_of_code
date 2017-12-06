#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <sstream>

using std::vector;
using std::string;

std::string to_s(const vector<int>& input, int highlight_index = -1) {
  std::ostringstream oss;
  for (int i = 0; i < input.size(); i++) {
    if (i == highlight_index) {
      oss << "(" << input[i] << ") ";
    }
    else {
      oss << input[i] << " ";
    }
  }
  return oss.str();
}

// perform the next step by computing the next jump target and incrementing the current cell
void next_step(int& current, vector<int>& work) {
  int next = current + work[current];
  work[current] = work[current] + 1;
  current = next;
}

// compute the number of jumps needed to escape the maze
int num_jumps(vector<int>& input) {
  int current = 0;
  int n = 0;
  int size = input.size();
  while (current < size) {
    //std::cerr << "size: " << size << ", n: " << n << ", current: " << current << ", input: " << to_s(input, current) << "\n";
    ++n;
    next_step(current, input);
  }
  return n;
}

void assert_equals(const vector<int>& expected, const vector<int>& actual, const string& msg) {
  if (to_s(expected) == to_s(actual)) {
    std::cout <<  ".";
  }
  else {
    std::cout << "FAILED (" << msg << "): expected " << to_s(expected) << ", got " << to_s(actual) << "\n";
  }
}

void assert_equals(const int& expected, const int& actual, const string& msg) {
  if (expected == actual) {
    std::cout <<  ".";
  }
  else {
    std::cout << "FAILED (" << msg << "): expected " << expected << ", got " << actual << "\n";
  }
}

void num_jumps_for_single_elem_vector_should_return_1() {
  int helper[1] = { 1 };
  vector<int> input (helper, helper+1);
  assert_equals(1, num_jumps(input), "simple");
}

void next_step_for_example_should_correctly_set_offsets_and_current() {
  int helper[5] = { 0, 3, 0, 1, -3 };
  vector<int> input (helper, helper+5);
  int current = 0;
  next_step(current, input);
  int helper2[5] = { 1, 3, 0, 1, -3 };
  vector<int> expected (helper2, helper2+5);
  assert_equals(expected, input, "offsets");
  assert_equals(0, current, "current");

}

void next_step_for_example_step3_should_correctly_set_offsets_and_current() {
  int helper[5] = { 2, 3, 0, 1, -3 }; 
  vector<int> input (helper, helper+5);
  int current = 1;
  next_step(current, input);
  int helper2[5] = { 2, 4, 0, 1, -3 }; 
  vector<int> expected (helper2, helper2+5);
  assert_equals(expected, input, "offsets");
  assert_equals(4, current, "current");
}


void num_jumps_for_example_should_return_5() {
  int helper[5] = { 0, 3, 0, 1, -3 };
  vector<int> input (helper, helper+5);
  assert_equals(5, num_jumps(input), "num jumps for example");
}

// read the input file into a vector of ints
std::vector<int> read_input() {
  std::ifstream is("input.txt");
  std::istream_iterator<int> start(is), end;
  std::vector<int> numbers(start, end);
  /*std::cout << "Read " << numbers.size() << " numbers" << std::endl;

  // print the numbers to stdout
  std::cout << "numbers read in:\n";
  std::copy(numbers.begin(), numbers.end(), 
            std::ostream_iterator<int>(std::cout, " "));
  std::cout << std::endl;
  */
  return numbers;
}

int main() {
  num_jumps_for_single_elem_vector_should_return_1();
  next_step_for_example_should_correctly_set_offsets_and_current();
  next_step_for_example_step3_should_correctly_set_offsets_and_current();
  num_jumps_for_example_should_return_5();
  std::vector<int> input = read_input();
  std::cout << "num jumps for part I: " << num_jumps(input) << "\n";
  return 0;
}

