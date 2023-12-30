import logging
import itertools as it

class Block:
    def __init__(self, name, start_idx, end_idx, map):
        self.name, self.start_idx, self.end_idx, self.map = name, start_idx, end_idx, map

    def __str__(self):
        return f"{self.name}\n{[str(me) for me in self.map]}\n"

class MapEntry:
    def __init__(self, dest_start, src_start, length):
        self.dest_start, self.src_start, self.length = dest_start, src_start, length

    def __str__(self):
        return f"{self.dest_start} {self.src_start} {self.length}"

class Solve:
    def __init__(self, lines):
        self.lines = lines
        # first line: seeds: s1 s2 s3 ...
        self.seeds = [int(s) for s in lines[0].split(":")[1].split()]
        # parse the blocks
        # this is quick'n dirty - we don't check the names and we assume we always get exactly 7 blocks
        b1 = Solve.parse_block(lines, 2)
        b2 = Solve.parse_block(lines, b1.end_idx+1)
        b3 = Solve.parse_block(lines, b2.end_idx+1)
        b4 = Solve.parse_block(lines, b3.end_idx+1)
        b5 = Solve.parse_block(lines, b4.end_idx+1)
        b6 = Solve.parse_block(lines, b5.end_idx+1)
        b7 = Solve.parse_block(lines, b6.end_idx+1)
        self.blocks = [b1, b2, b3, b4, b5, b6, b7]
        logging.info(f"blocks: {[str(b) for b in self.blocks]}")

    def parse_block(lines, start_idx):
        name = lines[start_idx]
        logging.info(f"parsing block {name}")
        block = []
        end_idx = None
        for i in range(start_idx+1, len(lines)):
            if lines[i] == "":
                end_idx = i
                break
            else:
                vals = [int(x) for x in lines[i].split()]
                block.append(MapEntry(vals[0], vals[1], vals[2]))
        if end_idx == None:
            end_idx = len(lines)+1
        return Block(name, start_idx, end_idx, block)

    def read_input_file(filename):
        with open(filename) as f:
            lines = [l.rstrip() for l in f.readlines()]
        return Solve(lines)

    def solve(self, paths):
        print(f"start: {paths}")
        # walk the path: Seed > soil > fertilizer > water > light > temperature > humidity > location
        for b in self.blocks:
            # compute next step for each path
            for p in paths:
                src = p[-1]
                dest = None
                for me in b.map:
                    if me.src_start <= src <= me.src_start+me.length-1:
                        dest = me.dest_start+(src-me.src_start)
                # no matching entry found? default: dest = src
                if dest == None:
                    dest = src
                p.append(dest)
            print(f"step: {paths}")
        return min([p[-1] for p in paths]) 

    def solve_part_I(self):
        paths = [[s] for s in self.seeds]
        return self.solve(paths)

    def solve_part_II(self):
        paths = []
        for i in range(0, len(self.seeds), 2):
            for j in range(self.seeds[i], self.seeds[i]+self.seeds[i+1]-1):
                paths.append([j])
        return self.solve(paths) 

if __name__ == '__main__':
    logging.basicConfig(level=logging.DEBUG)
    solve = Solve.read_input_file('input.txt')
    #print("{} {}".format(solve.solve_part_I(), solve.solve_part_II()))
    print("{} {}".format(solve.solve_part_II()))


