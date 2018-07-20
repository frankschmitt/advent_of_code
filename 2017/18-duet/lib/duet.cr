class Duet
  @@re_instruction = /^([a-z]+) ([a-z])( -?[0-9]+)?$/

  @registers = Hash(String, Int32).new

  property registers

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

  def set(reg : String, value : Int32)
    @registers[reg] = value
  end

  def add(reg : String, value : Int32)
    @registers[reg] += value
  end

  def mul(reg : String, value : Int32)
    @registers[reg] *= value
  end

  def mod(reg : String, value : Int32)
    @registers[reg] = @registers[reg] % value
  end

  def parse(input)
    input.each do |line|
      puts "parsing '#{line}'"
      md = @@re_instruction.match(line) 
      if md
        cmd = md[1]
        reg = md[2]
        if md[3]
          val = md[3].to_i 
          set(reg, val) if cmd == "set"
          add(reg, val) if cmd == "add"
          mul(reg, val) if cmd == "mul"
          mod(reg, val) if cmd == "mod"
        end
      end
    end 
  end

end
