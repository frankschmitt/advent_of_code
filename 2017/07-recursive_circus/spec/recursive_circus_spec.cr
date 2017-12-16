require "spec"
require "recursive_circus"

describe "RecursiveCircus" do

  describe "#parse_node" do
    rc = RecursiveCircus.new()

    it "extracts the node name and weight from a node without children" do
      input = "ktlj (57)"
      expected = Node.new("ktlj", 57, [] of String)
      rc.parse_node(input).should eq expected     
    end

    it "extracts the node name, weight and children from a full entry" do
      input = "fwft (72) -> ktlj, cntj, xhth"
      expected = Node.new("fwft", 72, ["ktlj", "cntj", "xhth"])
      rc.parse_node(input).should eq expected     
    end
  end


  describe "#find_root" do
    rc = RecursiveCircus.new()

    it "correctly finds the root for the example" do
      f = File.read_lines("sample_input.txt") 
      expected = Node.new("tknk", 41, ["ugml", "padx", "fwft"]) 
      rc.find_root(f).should eq expected
    end
    
    it "solves part I of the puzzle" do
      f = File.read_lines("input.txt") 
      # we've got to handle the nil case (although it should never happen)
      got = case root = rc.find_root(f)
        when Node 
          root.name
        else
          "nil"
        end
      got.should eq "mwzaxaj"
    end

  end

  describe "#build_tree" do
    rc = RecursiveCircus.new()

    it "builds a leaf node" do
      input = "ktlj (57)"
      expected = Tree.new(Node.new("ktlj", 57, [] of String), [] of Tree)
      rc.build_tree([input]).should eq expected
    end

    it "builds the sample tree" do
      f = File.read_lines("sample_input.txt") 
      expected_root = Node.new("tknk", 41, ["ugml", "padx", "fwft"])
      got = rc.build_tree(f)
      got.root.should eq expected_root
      got.to_s.should eq "tknk (41) -> ugml,padx,fwft"
    end
  end 

  describe "#compute_weight" do
    rc = RecursiveCircus.new()

    it "computes the weight of a leaf node" do
      input = "ktlj (57)"
      expected = 57
      tree = rc.build_tree([input])
      tree.weight.should eq expected
    end
    
    it "computes the weight of the sample tree" do
      f = File.read_lines("sample_input.txt") 
      tree = rc.build_tree(f)
      tree.weight.should eq (41 + 251 + 243 + 243) # node weight + child weight
    end
  end

  describe "#find_unbalanced" do
    rc = RecursiveCircus.new()

    it "finds the unbalanced node in the sample tree" do
      f = File.read_lines("sample_input.txt") 
      tree = rc.build_tree(f)
      rc.find_unbalanced(tree).should eq "tknk (41) -> ugml(251),padx(243),fwft(243)" 
    end

    it "solves part II of the puzzle" do
      f = File.read_lines("input.txt") 
      tree = rc.build_tree(f)
      rc.find_unbalanced(tree).should eq "ihnus (40) -> vrgxe(2166),shnqfh(2159),auzded(2159),hkhsc(2159),jwddn(2159),mcxki(2159),lhwyt(2159)"
      # the answer is then 1219 - vrgxe's base weight is 1226, and it is too heavy by 7
    end
  end
end
