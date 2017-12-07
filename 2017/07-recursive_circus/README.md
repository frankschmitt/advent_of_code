# Synopsis
Solution for the recursive circus challenge, using only the Bash shell and UNIX command-line tools

We exploit the fact that the root node is the only node that never appears on the right side of a node entry. Using that knowledget, the general approach is:
- extract the list of all nodes using awk
- extract the list of all child lists using awk
- for each node: check whether it exists in the child list (using bash / grep)


