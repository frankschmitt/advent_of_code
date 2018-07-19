class Duet
  #@@re_node = /([a-z]+) \(([0-9]+)\)/ # matches eg. fwft (72) 

#  # parse a node line, and return a new Node instance
#  def parse_node(input : String)
#    # parse name + weight
#    node_md = @@re_node.match(input)
#    name, weight = "?", 0
#    if node_md 
#      name = node_md[1]
#      weight = node_md[2].to_i
#    end 
#    # parse children
#    children = [] of String
#    if input.includes?("->")
#      _, x = input.split("->")
#      children = x.delete(" ").split(",")
#    end 
#    Node.new(name, weight, children)
#  end
#
#  # parse input, returning the list of Node instances (no tree building so far)
#  def parse_input(input)
#    nodes = [] of Node
#    input.each do |line|
#      nodes.push(parse_node(line))
#    end     
#    nodes
#  end
#
  def registers
    [] of String
  end
end
