# Synopsis
Solution for the digital plumber challenge, using [Ruby|http://www.ruby-lang.org].

# General approach
Pretty straightforward:
- read the input file
- extract + store the list of edges; we "sort" edges (v\_m, v\_n) so that m <= n (i.e. the smaller index comes first)
- remove duplicate edges
- create the initial list of groups: each group contains exactly one vertex
- for each edge (v\_m, v\_m): merge the groups containing v\_m and v\_n 

# Installation
Just follow the installation instructions on the Ruby homepage; then, install RSpec:
```
gem install rspec
```

## Running the tests
rspec solve.rb
