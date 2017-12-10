# Synopsis
Solution for the "I heard you like registers" challegen, in Lua.

# General approach
Pretty straighforward - use a table (specialty of Lua, a mixture between an array and a hash) to keep track of the current state, and parse + execute instructions one-by-one.

# Installation
Follow the instructions on the Lua homepage. This should install the Lua interpreter and the Luarocks package manager. To install the Luaunit testing framework:
```luarocks install luaunit```

# Running the tests
lua solve.lua 
