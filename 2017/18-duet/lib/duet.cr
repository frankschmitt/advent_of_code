class Duet
  @@re_instruction_2 = /^([a-z]+) ([a-z])$/
  @@re_instruction_3 = /^([a-z]+) ([a-z]) (-?[0-9]+)$/

  @registers = Hash(String, Int32).new
  @last_played = -1
  @instruction_pointer = -1

  property registers
  property last_played
  property instruction_pointer

  def debug(msg)
    puts msg
  end

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

  def snd(reg : String)
    @last_played = @registers[reg]
  end

  def instruction_2(match_data)
    cmd = match_data[1]
    reg = match_data[2]
    snd(reg) if cmd == "snd"
  end

  def instruction_3(match_data)
    cmd = match_data[1]
    reg = match_data[2]
    val = match_data[3].to_i 
    set(reg, val) if cmd == "set"
    add(reg, val) if cmd == "add"
    mul(reg, val) if cmd == "mul"
    mod(reg, val) if cmd == "mod"
  end

  def parse(input)
    input.each do |line|
      debug "parsing '#{line}'"
      md3 = @@re_instruction_3.match(line) 
      if md3
        instruction_3(md3)
      else
        md2 = @@re_instruction_2.match(line) 
        if md2
          instruction_2(md2) 
        end
      end
    end 
  end

end
