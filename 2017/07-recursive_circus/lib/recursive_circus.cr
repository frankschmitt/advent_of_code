class Node
  property name, weight, child_names

  def initialize(name : String, weight : Int32, child_names : Array(String))
    @name, @weight, @child_names = name, weight, child_names
  end

  def ==(other : Node)
    @name == other.name && @weight == other.weight && @child_names == other.child_names
  end
end


class Tree
  property root, children

  def initialize(root : Node, children : Array(Tree))
    @root, @children = root, children
  end

  def ==(other : Tree)
    @root == other.root && children == other.children
  end

  def to_s
    "#{root.name} (#{root.weight}) -> #{children.map {|c| c.root.name}.join(',') }"
  end
end

class RecursiveCircus
  @@re_node = /([a-z]+) \(([0-9]+)\)/ # matches eg. fwft (72) 

  # parse a node line, and return a new Node instance
  def parse_node(input : String)
    # parse name + weight
    node_md = @@re_node.match(input)
    name, weight = "?", 0
    if node_md 
      name = node_md[1]
      weight = node_md[2].to_i
    end 
    # parse children
    children = [] of String
    if input.includes?("->")
      _, x = input.split("->")
      children = x.delete(" ").split(",")
    end 
    Node.new(name, weight, children)
  end

  # parse input, returning the list of Node instances (no tree building so far)
  def parse_input(input)
    nodes = [] of Node
    input.each do |line|
      nodes.push(parse_node(line))
    end     
    nodes
  end
  
  # parse input (given as an array of strings), and return the root node
  def find_root(input)
    # extract list of nodes
    nodes = parse_input(input)
    # build shallow list of children
    all_children = nodes.map {|n| n.child_names}.flatten
    # find the node that is not a child (= the root node)
    nodes.find { |n| !all_children.includes?(n.name) }
  end


end
