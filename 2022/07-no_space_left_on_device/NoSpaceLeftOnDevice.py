from parse import *

class NoSpaceLeftOnDevice:

    def __init__(self, lines):
        # for each directory, we store the list of files plus their accumulated size
        self.entries = {}
        dir_stack = []
        for l in lines:
            # cd ? move within directory stack
            if l[0:4] == '$ cd':
                if l[5:] == '/':
                    dir_stack = ['/']
                elif l[5:] == '..':
                    dir_stack.pop()
                else:
                    dir_stack.append(l[5:])
            # ls? ignore it
            elif l[0:4] == '$ ls':
                pass # 
            # in ls output? push it unless it's a directory
            else:
                cwd = '/'.join(dir_stack).replace('//', '/')
                if l[0:3] == 'dir':
                    type = 'dir'
                    size = None
                    name = l[4:]
                else:
                    res = parse("{:d} {}", l)
                    type = 'file'
                    size = res[0]
                    name = res[1]
                path = (cwd + '/' + name).replace('//', '/')
                entry = {'type': type, 'size': size, 'name': name, 'path': path}
                # I'm sure there must be a more elegant way to do this, but setdefault seems useless ?!
                if cwd in self.entries.keys():
                    self.entries[cwd].append(entry)
                else:
                    self.entries[cwd] = [entry]
            #print("after parsing {}: dir_stack = {}, entries = {}".format(l, dir_stack, self.entries))
        #print("after init: {}".format(self.entries))

    def rec_size(self, entry):
        base_size = sum([e['size'] for e in self.entries[entry] if e['type'] == 'file'])
        child_size = sum([self.rec_size(e['path']) for e in self.entries[entry] if e['type'] == 'dir'])
        return base_size + child_size


    def read_input_file(filename):
        with open(filename) as f:
            lines = [l.rstrip() for l in f.readlines()]
        return NoSpaceLeftOnDevice(lines) 

    def solve_part_I(self):
        candidates = [(e, self.rec_size(e)) for e in self.entries if self.rec_size(e) <= 100000]
        return sum([c[1] for c in candidates])

    def solve_part_II(self):
        return -1


if __name__ == '__main__':
    ns = NoSpaceLeftOnDevice.read_input_file('input.txt')
    print("{} {}".format(ns.solve_part_I(), ns.solve_part_II()))
