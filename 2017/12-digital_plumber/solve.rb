class DigitalPlumber

  def initialize(input)
    parse_input(input)
    find_groups
  end

  def parse_input(input)
    @graph = Hash.new()
    re = /^([0-9]+) <-> ([0-9, ]+)/ 
    input.each do |line|
      md = re.match(line)
      node = md[1].to_i
      neighbours = md[2].split(",").map {|s| s.to_i}
      # we store edges and - for easier lookup always use the lower index first
      
    end 
  end

  def find_groups
    @groups = []
    @graph.each do |k,v|
      @groups.push([k]) # single-element group 
    end
  end

  def group_for_id(id) 
    @groups.detect {|g| g.include? id }
  end
end

describe "Digital Plumber" do
  it "returns a one-element group for a single input" do
    conns = [ "1 <-> 1"]
    dp = DigitalPlumber.new(conns)
    expect(dp.group_for_id(1)).to eq([1])
  end

  it "finds the groups for the sample input" do
    conns = IO.readlines("sample_input.txt")
    dp = DigitalPlumber.new(conns)
    expect(dp.group_for_id(0).count).to eq(6)
  end
end
