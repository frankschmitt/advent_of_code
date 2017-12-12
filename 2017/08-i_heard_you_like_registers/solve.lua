-- print a table
function table.print(t)
  for key,value in pairs(t) do print(key,value) end
end

-- get the key and value for the max value in a table
-- idea taken from https://stackoverflow.com/a/5180611/610979
function max(t)
  res = nil
  for key,value in pairs(t) do 
    if res == nil or res < value then
      res = value
    end
  end
  return res
end

-- taken from https://stackoverflow.com/a/641993/610979
function table.shallow_copy(t)
  local t2 = {}
  for k,v in pairs(t) do
    t2[k] = v
  end
  return t2
end

-- taken from http://lua-users.org/wiki/SplitJoin
function string:split(sSeparator, nMax, bRegexp)
  assert(sSeparator ~= '')
  assert(nMax == nil or nMax >= 1)

  local aRecord = {}

  if self:len() > 0 then
    local bPlain = not bRegexp
    nMax = nMax or -1

    local nField, nStart = 1, 1
    local nFirst,nLast = self:find(sSeparator, nStart, bPlain)
    while nFirst and nMax ~= 0 do
      aRecord[nField] = self:sub(nStart, nFirst-1)
      nField = nField+1
      nStart = nLast+1
      nFirst,nLast = self:find(sSeparator, nStart, bPlain)
      nMax = nMax-1
    end
    aRecord[nField] = self:sub(nStart)
  end
  return aRecord
end

-- given the current state, evaluate the condition "<var> <op> <val>"
--    e.g. "b < 5" returns true if b is less than 5 
--    if a variable has never been initialize, it is assumed to be 0
function evaluateCondition(state, var, op, val)
  local x = state[var] 
  local n = tonumber(val)
  if (x == nil) then
    x = 0 -- default value: 0 
  end
  if op == "==" then
    return x == n
  elseif op == "!=" then
    return x ~= n
  elseif op == ">" then
    return x > n
  elseif op == ">=" then
    return x >= n
  elseif op == "<" then
    return x < n
  elseif op == "<=" then
    return x <= n
  else
    error("unknown operator" + op)
  end
end

-- compute new state from old state and input
-- @return new state (as a table)
function registers( input, old_state  ) 
  local new_state = table.shallow_copy(old_state)
  -- Format: operand operator offset if argument comparison value
  --    e.g. 'a inc 2 if b < 5'
  local ops = string.split(input, ' ')
  if evaluateCondition(old_state, ops[5], ops[6], ops[7]) then
    local old_val = old_state[ops[1]]
    -- use 0 as default 
    if old_val == nil then
      old_val = 0
    end
    -- compute new value
    if ops[2] == "inc" then
      new_state[ops[1]] = old_val + ops[3]
    elseif ops[2] == "dec" then
      new_state[ops[1]] = old_val - ops[3]
    else
      error("unknown inc/dec operator: " + ops[2])
    end
  end
  -- new_state["a"] = 2
  return new_state
end

-- load a program from file, evaluate it, and print the result
function runProgram(filename) 
  local file = io.open(filename)
  local result = {}
  if file then
    for line in file:lines() do 
      result = registers(line, result)
    end
  end
  return max(result)
end

-- Unit testing starts
lu = require('luaunit')

TestSplitString = {} -- class
function TestSplitString:testSimpleSplit()
  result = string.split("hello from Lua", ' ')
  lu.assertEquals( type(result), 'table' )
  lu.assertEquals( result[1], "hello" )
  lu.assertEquals( result[2], "from" )
  lu.assertEquals( result[3], "Lua" )
end

TestEvaluateCondition = {}
function TestEvaluateCondition:testEqualsWithExistingVar()
  local state = {}
  state["a"] = 2
  lu.assertEquals(evaluateCondition(state, "a", "==", "2"), true)
  lu.assertEquals(evaluateCondition(state, "a", "==", "3"), false)
end
function TestEvaluateCondition:testNotEqualsWithExistingVar()
  local state = {}
  state["a"] = 2
  lu.assertEquals(evaluateCondition(state, "a", "!=", "2"), false)
  lu.assertEquals(evaluateCondition(state, "a", "!=", "3"), true)
end

TestMax = {} --class
function TestMax:testMaxForUnsortedTable()
  local input = {}
  input["a"] = 5
  input["b"] = 13
  input["c"] = 12
  lu.assertEquals(max(input), 13)
end
TestRegister = {} --class
function TestRegister:testSingleInstructionShouldSetValIfConditionIsTrue()
  input = 'a inc 2 if b < 5'
  result = registers(input, {})
  lu.assertEquals( type(result), 'table' )
  lu.assertEquals( result["a"], 2 )
end

function TestRegister:testSingleInstructionShouldNotSetValIfConditionIsTrue()
  local input = 'a inc 2 if b > 5'
  local result = registers(input, {})
  lu.assertEquals( type(result), 'table' )
  lu.assertEquals( result["a"], nil )
end

function TestRegister:testMaxValueAfterSampleProgram()
  lu.assertEquals(1, runProgram("sample_input.txt"))
end

TestSolve = {}
function TestSolve:testSolutionForPartI() 
  lu.assertEquals(6611, runProgram("input.txt"))
end

os.exit(lu.LuaUnit.run())
