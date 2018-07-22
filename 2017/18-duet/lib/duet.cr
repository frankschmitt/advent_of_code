class Duet
  @@re_instruction_2 = /^([a-z]+) ([a-z])$/
  @@re_instruction_3 = /^([a-z]+) ([a-z]) (-?[0-9]+)$/

  @registers = Hash(String, Int32).new
  @last_played = -1
  @instruction_pointer = 0

  property registers
  property last_played
  property instruction_pointer

  def debug(msg)
    puts msg
  end

  # handlers for specific instructions
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
    @instruction_pointer += 1
  end

  def instruction_3(match_data)
    cmd = match_data[1]
    reg = match_data[2]
    val = match_data[3].to_i 
    set(reg, val) if cmd == "set"
    add(reg, val) if cmd == "add"
    mul(reg, val) if cmd == "mul"
    mod(reg, val) if cmd == "mod"
    if cmd == "jgz"
      if @registers[reg] > 0
        @instruction_pointer += val
      else
        @instruction_pointer += 1
      end
    else
      @instruction_pointer += 1
    end
  end

  # parse the input list of instructions, and run the program
  def run(input)
    while @instruction_pointer >= 0 && @instruction_pointer < input.count { |e| true } 
      #input.each do |line|
      line = input[@instruction_pointer]
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
