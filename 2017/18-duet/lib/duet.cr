class Duet
  @@re_instruction_2 = /^([a-z]+) ([a-z])$/
  #@@re_instruction_3 = /^([a-z]+) ([a-z]) (-?[0-9]+)$/
  @@re_instruction_3 = /^([a-z]+) ([a-z]) ([a-z]|-?[0-9]+)$/

  @registers = Hash(String, Int64).new(Int64.new(0))
  @last_played = Int64.new(-1)
  @last_received = Int64.new(-1)
  @instruction_pointer = Int64.new(0)

  property registers
  property last_played
  property last_received
  property instruction_pointer

  def debug(msg)
    #puts msg
  end

  # handlers for specific instructions
  def set(reg : String, value : Int64)
    @registers[reg] = value
  end

  def add(reg : String, value : Int64)
    @registers[reg] += value
  end

  def mul(reg : String, value : Int64)
    @registers[reg] *= value
  end

  def mod(reg : String, value : Int64)
    @registers[reg] = @registers[reg] % value
  end

  def snd(reg : String)
    @last_played = @registers[reg]
  end

  def rcv(reg : String)
    if @registers[reg] != 0
      @last_received = @last_played
      @instruction_pointer = -10 #terminate
    end
  end

  def instruction_2(match_data)
    cmd = match_data[1]
    reg = match_data[2]
    snd(reg) if cmd == "snd"
    rcv(reg) if cmd == "rcv"
    @instruction_pointer += 1
  end

  def is_numeric(s)
   /^-?[0-9]+$/.match(s) 
  end
  
  def instruction_3(match_data)
    cmd = match_data[1]
    reg = match_data[2]
    if is_numeric(match_data[3])  
      val = Int64.new(match_data[3].to_i)
    else
      val = @registers[match_data[3]]
    end
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

  def step(input)
    line = input[@instruction_pointer]
    debug "executing '#{line}'"
    md3 = @@re_instruction_3.match(line) 
    if md3
      instruction_3(md3)
    else
      md2 = @@re_instruction_2.match(line) 
      if md2
        instruction_2(md2) 
      end
    end
    debug "  #{self.to_s}"
  end

  # parse the input list of instructions, and run the program
  def run(input)
    while @instruction_pointer >= 0 && @instruction_pointer < input.count { |e| true } 
      step(input)
    end 
  end

  def to_s
    "ip: #{@instruction_pointer} registers: #{@registers} last_played: #{@last_played}"
  end

end


