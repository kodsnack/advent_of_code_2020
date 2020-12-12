direction = 90
x = 0
y = 0

def move(xs,ys):
    global x, y
    x += xs
    y += ys

def rotate(deg):
    global direction
    direction = (direction + deg) % 360

def move_forward(v):
    global x,y
    if direction == 0:
        y += v
    elif direction == 90:
        x += v
    elif direction == 180:
        y -= v
    elif direction == 270:
        x -= v

def process(instr):
    i = instr[:1]
    value = int(instr[1:])
    {
        "N": lambda x: move(0, x),
        "E": lambda x: move(x, 0),
        "S": lambda x: move(0, -x),
        "W": lambda x: move(-x, 0),
        "L": lambda x: rotate(-x),
        "R": lambda x: rotate(x),
        "F": lambda x: move_forward(x)
    }[i](value)

instruction_list = []
with open('input.txt') as f:
    instruction_list = [l.strip() for l in f]

for instr in instruction_list:
    process(instr)
print("Part 1:", abs(x)+abs(y))