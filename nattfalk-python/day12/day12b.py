import math

x = 0
y = 0
wpx = 10
wpy = 1

def move_wp(xs,ys):
    global wpx, wpy
    wpx += xs
    wpy += ys

def rotate(a):
    global wpx, wpy
    radians = -(math.pi / 180) * a
    wpx2 = round(math.cos(radians) * wpx - math.sin(radians) * wpy)
    wpy = round(math.sin(radians) * wpx + math.cos(radians) * wpy)
    wpx = wpx2

def move_forward(v):
    global x,y
    x += wpx * v
    y += wpy * v

def process(instr):
    i = instr[:1]
    value = int(instr[1:])
    {
        "N": lambda x: move_wp(0, x),
        "E": lambda x: move_wp(x, 0),
        "S": lambda x: move_wp(0, -x),
        "W": lambda x: move_wp(-x, 0),
        "L": lambda x: rotate(-x),
        "R": lambda x: rotate(x),
        "F": lambda x: move_forward(x)
    }[i](value)

instruction_list = []
with open('input.txt') as f:
    instruction_list = [l.strip() for l in f]

for instr in instruction_list:
    process(instr)
print("Part 2:", abs(x)+abs(y))

