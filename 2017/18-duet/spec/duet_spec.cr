require "spec"
require "duet"

describe "Duet" do

  it "initializes the set of registers to empty" do
    duet = Duet.new()
    expected = {} of String => Int64
    duet.registers.should eq expected     
  end

  it "sets the register for a set instruction" do
    duet = Duet.new()
    input = [ "set a 3" ]
    expected = {"a" => 3} 
    duet.run(input)
    duet.registers.should eq expected     
  end

  it "adds to a register for the add instruction" do
    duet = Duet.new()
    duet.registers = { "a" => Int64.new(3) }
    input = [ "add a 2" ]
    expected = { "a" => 5 } 
    duet.run(input)
    duet.registers.should eq expected     
  end

  it "multiplies a register with a constant for the mul instruction" do
    duet = Duet.new()
    duet.registers = { "a" => Int64.new(3) }
    input = [ "mul a 2" ]
    duet.run(input)
    duet.registers.should eq ({ "a" => 6 })
  end

  it "multiplies a register with a register for the mul instruction" do
    duet = Duet.new()
    duet.registers = { "a" => Int64.new(3) }
    input = [ "mul a a" ]
    duet.run(input)
    duet.registers.should eq ({ "a" => 9 })
  end

  it "sets a register to the remainder for the mod instruction" do
    duet = Duet.new()
    duet.registers = { "a" => Int64.new(13) }
    input = [ "mod a 6" ]
    duet.run(input)
    duet.registers.should eq ({ "a" => 1 }) 
  end

  it "sets the last played sound for the snd instruction" do
    duet = Duet.new()
    duet.registers = { "a" => Int64.new(16) }
    input = [ "snd a" ]
    duet.run(input)
    duet.last_played.should eq 16
  end

  it "does not change the instruction pointer if jgz condition is false" do
    duet = Duet.new()
    input = [ "set a 0", "jgz a -1", "set b 0" ]
    duet.run(input)
    duet.instruction_pointer.should eq 3 # one after end of program 
    duet.registers.should eq ({ "a" => 0, "b" => 0 })
  end

  it "sets the instruction pointer if jgz condition is true" do
    duet = Duet.new()
    input = [ "set a 7", "jgz a 3", "set b 0" ]
    duet.run(input)
    duet.instruction_pointer.should eq 4 
    duet.registers.should eq ({ "a" => 7 }) # b has never been set
  end

  it "uses the instruction pointer to determine the next instruction" do
    duet = Duet.new()
    input = [ "set a 7", "set b 2" ]
    duet.instruction_pointer = 1
    duet.run(input)
    duet.registers.should eq ({ "b" => 2 })
  end

  it "sets the last received sound if rcv condition is true" do
    duet = Duet.new()
    input = ["set a 3", "set b 4", "snd a", "rcv b" ]
    duet.run(input)
    duet.last_received.should eq 3 
  end

  it "gives the correct result for the sample program" do
    duet = Duet.new()
    input = File.read_lines("sample_input.txt")
    duet.run(input)
    duet.last_received.should eq 4
  end

  it "solves part I" do
    duet = Duet.new()
    duet.run(File.read_lines("input.txt"))
    duet.last_received.should eq 3423
  end

end
