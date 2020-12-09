POS = 31

data_lines = open("input.txt", "r").read().splitlines()

px = 0
py = 0
trees = 0
while py < len(data_lines)-1:
    px += 3
    if px >= POS:
        px -= POS
    py += 1
    if data_lines[py][px] == '#':
        trees += 1

print(f'Hit {trees} trees!')