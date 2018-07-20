require "spec"
require "duet"

describe "Duet" do

  describe "#parse_node" do
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
      expected = { "a" => 6 } 
      duet.parse(input)
      duet.registers.should eq expected     
    end

    it "sets a register to the remainder for the mod instruction" do
      duet.registers = { "a" => 13 }
      input = [ "mod a 6" ]
      expected = { "a" => 1 } 
      duet.parse(input)
      duet.registers.should eq expected     
    end
  end

end
