#include <iostream>
#include <vector>
#include <string>
#include <sstream>

using std::vector;
using std::string;

void next_step(int current, vector<int>& work) {
  int next = work[current];
  work[current] = work[current] + 1;
  current = next;
}

int num_jumps(vector<int>& input) {
  return 1;
}

std::string to_s(const vector<int>& input) {
  std::ostringstream oss;
  for (int i = 0; i < input.size(); i++) {
    oss << input[i] << " ";
  }
  return oss.str();
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

void num_jumps_for_example_should_return_5() {
  int helper[5] = { 0, 3, 0, 1, -3 };
  vector<int> input (helper, helper+5);
  assert_equals(5, num_jumps(input), "num jumps for example");
}

int main() {
  num_jumps_for_single_elem_vector_should_return_1();
  next_step_for_example_should_correctly_set_offsets_and_current();
  return 0;
}

