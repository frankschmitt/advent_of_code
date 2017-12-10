-- Some super function to test
function registers( input  ) 
  result = {}
  result["a"] = 2
  return result
end

-- Unit testing starts
lu = require('luaunit')

TestRegister = {} --class
    function TestRegister:testSingleInstructionShouldSetValIfConditionIsTrue()
        input = 'a inc 2 if b < 5'
        result = registers(input)
        lu.assertEquals( type(result), 'table' )
        lu.assertEquals( result["a"], 2 )
    end


-- class TestMyStuff

-- LuaUnit:run()
os.exit(lu.LuaUnit.run())
