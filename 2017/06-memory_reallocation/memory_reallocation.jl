using Base.Test

# choose the next block for redistribution
# @return the value and index of the block to distribute (1 based)
#
function choose_block(x)
  findmax(x)
end

# redistribute the max block
# @return the redistributed memory
function redistribute(x)
  val, index = choose_block(x)
  result = x
  result[index] = 0 # zero redistributed block
  len = size(x)[1]
  # we distribute one-by-one until nothing is left
  for i = 1:val 
    index = (index == len) ? 1 : (index + 1) # wrap around at last index
    result[index] += 1
  end
  result
end

# tests for choose_block 
@test choose_block([0, 2, 7, 0]) == (7,3)
# choose_block should choose the first one if multiple max-value blocks exist
@test choose_block([0, 7, 0, 7]) == (7,2)

# tests for redistribution
@test redistribute([0, 0, 0, 1]) == [1, 0, 0, 0]
@test redistribute([0, 2, 7, 0]) == [2, 4, 1, 2]
