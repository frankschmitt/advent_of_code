
def inverse_captcha(n):
  sum = 0
  s = str(n)
  for index, elem in enumerate(s):
    # compare with next elem; at end of string: re-use first element
    if elem == s[(index + 1) % len(s)]:
      sum += int(elem)
  return sum


def inverse_captcha2(n):
  sum = 0
  s = str(n)
  offset = int(len(s) / 2)
  for index, elem in enumerate(s):
    # compare with next elem; at end of string: re-use first element
    if elem == s[(index + offset) % len(s)]:
      sum += int(elem)
  return sum

# UNIT TESTS - FIRST PART
def test_inverse_captcha_for_two_different_digits_equals_0():
  assert( inverse_captcha(12) == 0 )

def test_inverse_captcha_for_1122_equals_3():
  assert( inverse_captcha(1122) == 3 )

def test_inverse_captcha_for_1111_equals_4():
  assert( inverse_captcha(1111) == 4 )

def test_inverse_captcha_for_1234_equals_0():
  assert( inverse_captcha(1234) == 0 )

def test_inverse_captcha_for_91212129_equals_9():
  assert( inverse_captcha(91212129) == 9 )


# UNIT TESTS - SECOND PART
def test_inverse_captcha2_for_1212_equals_6():
  assert( inverse_captcha2(1212) == 6)

def test_inverse_captcha2_for_1221_equals_0():
  assert( inverse_captcha2(1221) == 0)

def test_inverse_captcha2_for_123425_equals_4():
  assert( inverse_captcha2(123425) == 4)

def test_inverse_captcha2_for_123123_equals_12():
  assert( inverse_captcha2(123123) == 12)

def test_inverse_captcha2_for_12131415_equals_4():
  assert( inverse_captcha2(12131415) == 4)
  

# SOLUTION
def test_solution():
  with open('input.txt', 'r') as myfile:
    data=myfile.read().replace('\n', '')
    assert (inverse_captcha(data) == 1047)
    assert (inverse_captcha2(data) == 982)

