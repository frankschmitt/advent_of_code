def debug(msg)
  # puts msg
end

class Duet
  # matches instruction with one argument, e.g. 'rcv a'
  @@re_instruction_2 = /^([a-z]+) ([a-z]|-?[0-9]+)$/
  # matches instruction with two arguments, e.g. 'jgz 1 7'
  @@re_instruction_3 = /^([a-z]+) ([a-z]|-?[0-9]+) ([a-z]|-?[0-9]+)$/

  @registers = Hash(String, Int64).new(Int64.new(0))
  @last_played = Int64.new(-1)
  @last_received = Int64.new(-1)
  @instruction_pointer = Int64.new(0)
  @snd_count = Int64.new(0)
  @input = [] of String
  @partner : (Duet | Nil) = nil
  @inbox = [] of Int64
  @id = -1

  property registers
  property last_played
  property last_received
  property instruction_pointer
  property snd_count
  property partner
  property input

  def initialize(id = -1)
    @id = id
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
    if is_numeric(reg)
      val = Int64.new(reg.to_i)
    else
      val = @registers[reg]
    end

    @last_played = val 
    @snd_count += 1
    case p = @partner 
      when Duet then
        p.to_inbox(val)
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
          debug("  decreasing ip")
          @instruction_pointer -= 1 # HACK (since it's incremented in the calling function afterwards
        else
          #@registers[reg] = @inbox.pop
          @registers[reg] = @inbox.shift
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
      if is_numeric(reg)
        val2 = Int64.new(reg.to_i)
      else
        val2 = @registers[reg]
      end
      if val2 > 0 
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
    debug "  before: #{self.to_s}"
    md3 = @@re_instruction_3.match(line) 
    if md3
      instruction_3(md3)
    else
      md2 = @@re_instruction_2.match(line) 
      if md2
        instruction_2(md2) 
      else
        debug "UNKNOWN INSTRUCTION: #{line}"
      end
    end
    debug "  after:  #{self.to_s}"
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
    has_valid_instruction_pointer? && (!receiving? || !@inbox.empty?)
  end
 
  # parse the input list of instructions, and run the program
  def run(input)
    @input = input
    while is_running?
      step()
    end 
  end

  def to_s
    "#{@id} ip: #{@instruction_pointer} registers: #{@registers} last_played: #{@last_played} inbox: #{@inbox}"
  end

end

class DuetRunner
  @duet0 = Duet.new(0)
  @duet1 = Duet.new(1)
   
  getter duet0
  getter duet1 
  
  def run(input)
    @duet0.registers = { "p" => Int64.new(0) }
    @duet1.registers = { "p" => Int64.new(1) }
    @duet0.partner = @duet1
    @duet1.partner = @duet0
    @duet0.input = input
    @duet1.input = input
    
    # terminate if both partners are in the receiving state and have an empty inbox
    while (@duet0.is_running? || @duet1.is_running? )
      @duet0.step
      @duet1.step
    end
  end
end
