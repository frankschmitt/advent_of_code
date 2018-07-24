class Duet
  @@re_instruction_2 = /^([a-z]+) ([a-z])$/
  @@re_instruction_3 = /^([a-z]+) ([a-z]) ([a-z]|-?[0-9]+)$/

  @registers = Hash(String, Int64).new(Int64.new(0))
  @last_played = Int64.new(-1)
  @last_received = Int64.new(-1)
  @instruction_pointer = Int64.new(0)
  @snd_count = Int64.new(0)
  @input = [] of String
  @partner : (Duet | Nil) = nil
  @inbox = [] of Int64

  property registers
  property last_played
  property last_received
  property instruction_pointer
  property snd_count
  property partner
  property input

  def debug(msg)
    puts msg
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
    @snd_count += 1
    case p = @partner 
      when Duet then
        p.to_inbox(@registers[reg])
    end
  end
 
  def to_inbox(val : Int64)
    @inbox.push(val) 
  end

  def rcv(reg : String)
    case p = @partner
      # Duet mode: wait for message in inbox to arrive
      when Duet then
        if @inbox.empty?
          @instruction_pointer -= 1 # HACK (since it's incremented in the calling function afterwards
        else
          @registers[reg] = @inbox.pop
        end
      # running standalone - check register, terminate if it's not equal zero
      else
        if @registers[reg] != 0
          @last_received = @last_played
          @instruction_pointer = -10 #terminate
        end
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

  def step()
    line = @input[@instruction_pointer]
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

  def has_valid_instruction_pointer?
    @instruction_pointer >= 0 && @instruction_pointer < @input.count { |e| true } 
  end

  def receiving?
    b = has_valid_instruction_pointer? && (@partner != nil) && (@input[@instruction_pointer][0,3] == "rcv")
    debug("receiving? #{b}")
    b
  end

  def is_running?
    has_valid_instruction_pointer? && !receiving?
  end
 
  # parse the input list of instructions, and run the program
  def run(input)
    @input = input
    while is_running?
      step()
    end 
  end

  def to_s
    "ip: #{@instruction_pointer} registers: #{@registers} last_played: #{@last_played}"
  end

end

class DuetRunner
  @duet0 = Duet.new()
  @duet1 = Duet.new()
   
  getter duet0
  getter duet1 
  
  def run(input)
    @duet0.registers = { "p" => Int64.new(0) }
    @duet1.registers = { "p" => Int64.new(1) }
    @duet0.partner = @duet1
    @duet1.partner = @duet0
    @duet0.input = input
    @duet1.input = input

    while @duet0.is_running? && @duet1.is_running?
      @duet0.step
      @duet1.step
    end
  end
end
