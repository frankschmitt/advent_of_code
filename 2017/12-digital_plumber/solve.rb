class DigitalPlumber
  attr_reader :edges

  def initialize(input)
    parse_input(input)
  end

  def parse_input(input)
    @edges = []
    re = /^([0-9]+) <-> ([0-9, ]+)/ 
    input.each do |line|
      md = re.match(line)
      node = md[1].to_i
      neighbours = md[2].split(",").map {|s| s.to_i}
      # we store edges and - for easier lookup always use the lower index first
      neighbours.each do |n|
        v1 = [node, n].min
        v2 = [node, n].max
        @edges.push([v1, v2])
      end
    end 
    @edges.uniq!
  end

  # initialize single-element groups (one group for each node)
  def initialize_groups
    @groups = []
    @edges.each do |e|
      @groups.push([e[0]]) # single-element group 
      @groups.push([e[1]]) # single-element group 
    end
    @groups.uniq!
  end

  # merge two groups
  # removes both groups from the list, and adds a new group which is the
  # merger of the groups
  def merge_groups(g1, g2)
    #puts "merging #{g1} and #{g2}"
    g = (g1 + g2).uniq # use uniq to handle the case when g1 == g2
    @groups.delete g1
    @groups.delete g2
    @groups.push g
    #puts "merged to #{g}"
  end

  # find all groups in the graph
  def find_groups
    initialize_groups
    @edges.each do |e|
      v1, v2 = e[0], e[1]
      g1 = group_for_id(v1)
      g2 = group_for_id(v2)
      merge_groups(g1, g2)
    end
  end

  def group_for_id(id) 
    find_groups if @groups == nil
    @groups.detect {|g| g.include? id }
  end
end

describe "Digital Plumber" do
  it "builds a list of edges from single-element input" do
    conns = [ "1 <-> 1"]
    dp = DigitalPlumber.new(conns)
    expect(dp.edges).to eq([[1, 1]])
  end

  it "builds a list of edges for the sample input" do
    conns = IO.readlines("sample_input.txt")
    dp = DigitalPlumber.new(conns)
    expect(dp.edges).to eq([
      [0, 2],
      [1, 1],
      [2, 3],
      [2, 4],
      [3, 4],
      [4, 6],
      [5, 6]
    ])
  end

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
