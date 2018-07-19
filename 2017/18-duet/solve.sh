#!/bin/sh

# usage: $0 <input file>

input=$1
# extract all nodes
awk -F " " '{print $1;}' $input > nodes.txt

# extract the lists of children
awk -F "->" '{print $2;}' $input > children.txt

# find the root
while read NODE
do
  grep -q "$NODE" children.txt || echo "root node: $NODE"
done < nodes.txt
