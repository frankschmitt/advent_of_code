# Synopsis
Solution for the Memory reallocation challenge, in Julia.

We use a dictionary (aka hash table) to keep track of the configurations we've encountered so far. The solution is pretty straightforward:
- find the maximum value
- redistribute it
- repeat until we get a configuration we've already seen

# Installation
Go to the [Julia homepages|http://www.julialang.org] and follow the installation instructions. No additional packages required.

# Running the tests + solution
```julia memory_reallocation.jl```
