POS = 31

data_lines = open("input.txt", "r").read().splitlines()

def calc_slope(xs, ys):
    px = 0
    py = 0
    trees = 0
    while py < len(data_lines)-1:
        px += xs
        if px >= POS:
            px -= POS
        py += ys
        if py >= len(data_lines):
            break
        if data_lines[py][px] == '#':
            trees += 1
    return trees

total = calc_slope(1,1) * calc_slope(3,1) * calc_slope(5,1) * calc_slope(7,1) * calc_slope(1,2)

print(f'Hit {total} trees!')