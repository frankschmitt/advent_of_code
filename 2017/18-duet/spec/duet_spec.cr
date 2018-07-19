require "spec"
require "duet"

describe "Duet" do

  describe "#parse_node" do
    duet = Duet.new()

    it "initializes the set of registers to empty" do
      expected = [] of String
      duet.registers.should eq expected     
    end

    it "correctly sets the register to 1 for a set instruction" do
      input = "set a 1"
      expected = [{:a => 1}] 
      duet.parse(input)
      duet.registers.should eq expected     
    end

  end

end
