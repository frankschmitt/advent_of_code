class Node
  property name, weight, child_names

  def initialize(name : String, weight : Int32, child_names : Array(String))
    @name, @weight, @child_names = name, weight, child_names
  end

  def ==(other : Node)
    @name == other.name && @weight == other.weight && @child_names == other.child_names
  end

  def to_s
    "#{name} (#{weight}) -> #{child_names.join(',')}"
  end
end


class Tree
  property root, children, weight : Int32, is_balanced : Bool

  def initialize(root : Node, children : Array(Tree))
    @root, @children = root, children
    @weight = root.weight + children.map {|c| c.weight}.sum
    @is_balanced = children.empty? || children.all? {|child| child.weight == children.first.weight }
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
  
  # given a list of nodes, return the root node
  def find_root_internal(nodes)
    # build shallow list of children
    all_children = nodes.map {|n| n.child_names}.flatten
    # find the node that is not a child (= the root node)
    nodes.find { |n| !all_children.includes?(n.name) }
  end

  # parse input (given as an array of strings), and return the root node
  def find_root(input)
    # extract list of nodes
    nodes = parse_input(input)
    find_root_internal(nodes)
  end

  # given a root node and a list of available nodes, return the (sub-)tree for the root node
  def build_tree_internal(root : Node, nodes : Array(Node))
    # find all nodes for the child names
    child_nodes = [] of Node
    root.child_names.each do |child_name| 
      child_nodes += nodes.select {|n| n.name == child_name }
    end
    # build child trees recursively
    child_trees = child_nodes.map {|child_node| build_tree_internal(child_node, nodes).as(Tree) }
    # finally, build the tree from the root node and the child trees
    Tree.new(root, child_trees)
  end

  # parse input (given as an array of strings), build tree and return it
  def build_tree(input)
    nodes = parse_input(input)
    tree = case root = find_root_internal(nodes)
      when Node 
        build_tree_internal(root, nodes)
      else
        # return a dummy Tree if we didn't find a root node
        Tree.new(Node.new("????", -1, [] of String), [] of Tree)
    end 
    tree
  end

  # find the deepest unbalanced sub-tree in a given tree
  # @return deepest unbalanced sub-tree if tree is not balanced, nil otherwise
  def find_unbalanced_int(tree)
    result = nil
    if !tree.is_balanced 
      result = tree 
      tree.children.each do |child|
        if subtree = find_unbalanced_int(child) 
          result = subtree
        end
      end
    end
    result 
  end

  # find the deepest unbalanced node in the given tree, and return a string that contains its name and base weight and 
  #   all its children with their respective sub-tree weight
  # The answer to the puzzle can then be extracted by 
  #   - finding the single child whose weight doesn't match 
  #   - getting its base weight from the input
  #   - adjusting this base weight by the difference found in step 1
  def find_unbalanced(tree)
    case t = find_unbalanced_int(tree)
      when Tree
        "#{t.root.name} (#{t.root.weight}) -> #{t.children.map {|c| c.root.name + '(' + c.weight.to_s + ')' }.join(',')}"
      else
        t.to_s
    end
  end   
end
