import re

class ChildNode:
    def __init__(self, count, color):
        self.count = count
        self.color = color

class Node:
    def __init__(self, color):
        self.color = color
        self.childnodes = []

nodes = []
with open('input.txt', 'r') as f:
    for line in f:
        line = line.strip()
        p1, p2 = line.split(' contain ', 2)
        result = re.match(r'(\w+\s\w+)\s(bags|bag)\.?', p1)
        n = Node(result.group(1))
        if p2 != 'no other bags.':
            for x in p2.split(', '):
                result = re.match(r'(\d)\s(\w+\s\w+)\s(bags|bag)\.?', x)
                c = ChildNode(int(result.group(1)), result.group(2))
                n.childnodes.append(c)
        nodes.append(n)

# Part 1
colors = []
prev_colors = ["shiny gold"]
while True:
    tmp_colors = []
    for n in nodes:
        for n2 in n.childnodes:
            for c in prev_colors:
                if c == n2.color:
                    tmp_colors.append(n.color)
    if len(tmp_colors) > 0:
        prev_colors = tmp_colors
        colors += tmp_colors
    else:
        break

print("Found", len(set(colors)), "unique bags that can contain 'shiny gold' bags")

# Part 2
root_node = (x for x in nodes if x.color == "shiny gold")
def recursive(n):
    if n == None: return 0
    count = 0
    for n2 in n:
        for c in n2.childnodes:
            next_nodes = (x for x in nodes if x.color == c.color)
            count += c.count + c.count * recursive(next_nodes)
    return count
    
total_count = recursive(root_node)
print("Gold bag can contain", total_count, "other bags")