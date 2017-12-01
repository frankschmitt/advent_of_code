
def inverse_captcha(n):
  #s = str(n) + str(n)[0] # append first digit at end
  sum = 0
  s = str(n)
  for index, elem in enumerate(s):
    # compare with next elem; at end of string: re-use first element
    if elem == s[(index + 1) % len(s)]:
      sum += int(elem)
  return sum


# UNIT TESTS
def test_inverse_captcha_for_two_different_digits_equals_0():
  assert( inverse_captcha(12) == 0 )

def test_inverse_captcha_for_1122_equals_3():
  assert( inverse_captcha(1122) == 3 )
