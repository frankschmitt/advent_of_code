# Synopsis
Solution for the recursive circus challenge. 
The original solution for part I used only the Bash shell and UNIX command-line tools; however, since I saw now way to solve part II in a similar fashion, I decided to do both parts in [Crystal|https://crystal-lang.org/].

# General approach (part I, first version)
We exploit the fact that the root node is the only node that never appears on the right side of a node entry. Using that knowledget, the general approach is:
- extract the list of all nodes using awk
- extract the list of all child lists using awk
- for each node: check whether it exists in the child list (using bash / grep)

# General approach (second version)
We exploit the fact that the root node is the only node that never appears on the right side of a node entry. Using that knowledget, the general approach is:
- extract the list of all nodes 
- extract the list of all child lists 
- for each node: check whether it exists in the child list 

# Installation
Just follow the installation instructions on the Crystal homepage

# Running the tests
Crystal comes with a built-in BDD framework; to run the tests:
```crystal spec```


