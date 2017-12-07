using Base.Test

# choose the next block for redistribution
# @return the value and index of the block to distribute (1 based)
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

# count redistributions until cycle is detected
# @return the number of redistributions
function count_until_cycle(x)
  seen = Dict()
  n = 0
  next = x
  while !haskey(seen, "$next")
    n += 1
    seen["$next"] = true
    next = redistribute(next)
  end
  n
end

# tests for choose_block 
@test choose_block([0, 2, 7, 0]) == (7,3)
# choose_block should choose the first one if multiple max-value blocks exist
@test choose_block([0, 7, 0, 7]) == (7,2)

# tests for redistribution
@test redistribute([0, 0, 0, 1]) == [1, 0, 0, 0]
@test redistribute([0, 2, 7, 0]) == [2, 4, 1, 2]

# tests for detecting cycles
@test count_until_cycle([0, 2, 7, 0]) == 5


# solve it
input = [11,11,13,7,0,15,5,5,4,4,1,1,7,1,15,11]
println(count_until_cycle(input))
