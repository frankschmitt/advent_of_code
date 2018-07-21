require "spec"
require "duet"

describe "Duet" do
  duet = Duet.new()

  it "initializes the set of registers to empty" do
    expected = {} of String => Int32
    duet.registers.should eq expected     
  end

  it "sets the register for a set instruction" do
    input = [ "set a 3" ]
    expected = {"a" => 3} 
    duet.parse(input)
    duet.registers.should eq expected     
  end

  it "adds to a register for the add instruction" do
    duet.registers = { "a" => 3 }
    input = [ "add a 2" ]
    expected = { "a" => 5 } 
    duet.parse(input)
    duet.registers.should eq expected     
  end

  it "multiplies to a register for the mul instruction" do
    duet.registers = { "a" => 3 }
    input = [ "mul a 2" ]
    duet.parse(input)
    duet.registers.should eq ({ "a" => 6 })
  end

  it "sets a register to the remainder for the mod instruction" do
    duet.registers = { "a" => 13 }
    input = [ "mod a 6" ]
    duet.parse(input)
    duet.registers.should eq ({ "a" => 1 }) 
  end

  it "sets the last played sound for the snd instruction" do
    duet.registers = { "a" => 16 }
    input = [ "snd a" ]
    duet.parse(input)
    duet.last_played.should eq 16
  end

  it "sets the instruction pointer if jgz condition is true" do
    input = [ "set a 7", "jgz a -1", "set b 0" ]
    duet.parse(input)
    duet.instruction_pointer.should eq 0 
  end

end
