def active_neighbors(state, x, y, z):
    count = 0

    for xx in range(x-1, x+2):
        for yy in range(y-1, y+2):
            for zz in range(z-1,z+2):
                if (xx,yy,zz) == (x,y,z):
                    continue
                if (xx,yy,zz, 0) in state:
                    count += 1
    return count

def active_neighbors_4d(state, x, y, z, w):
    count = 0

    for xx in range(x-1, x+2):
        for yy in range(y-1, y+2):
            for zz in range(z-1,z+2):
                for ww in range(w-1,w+2):
                    if (xx,yy,zz,ww) == (x,y,z,w):
                        continue
                    if (xx,yy,zz,ww) in state:
                        count += 1
    return count

def do_cycle(initial_state, cycles):
    state = initial_state
    for _ in range(cycles):
        new_state = set()
        minx = min([x for x,_,_,_ in state])
        maxx = max([x for x,_,_,_ in state])
        miny = min([y for _,y,_,_ in state])
        maxy = max([y for _,y,_,_ in state])
        minz = min([z for _,_,z,_ in state])
        maxz = max([z for _,_,z,_ in state])

        for z in range(minz-1, maxz+2):
            for y in range(miny-1, maxy+2):
                for x in range(minx-1, maxx+2):
                    neighbours = active_neighbors(state, x, y, z)
                    if ((x,y,z,0) in state and neighbours == 2) or neighbours == 3:
                        new_state.add((x,y,z,0))

        state = new_state
    return state

def do_cycle_4d(initial_state, cycles):
    state = initial_state
    for _ in range(cycles):
        new_state = set()
        minx = min([x for x,_,_,_ in state])
        maxx = max([x for x,_,_,_ in state])
        miny = min([y for _,y,_,_ in state])
        maxy = max([y for _,y,_,_ in state])
        minz = min([z for _,_,z,_ in state])
        maxz = max([z for _,_,z,_ in state])
        minw = min([w for _,_,_,w in state])
        maxw = max([w for _,_,_,w in state])

        for w in range(minw-1, maxw+2):
            for z in range(minz-1, maxz+2):
                for y in range(miny-1, maxy+2):
                    for x in range(minx-1, maxx+2):
                        neighbours = active_neighbors_4d(state, x, y, z, w)
                        if ((x,y,z,w) in state and neighbours == 2) or neighbours == 3:
                            new_state.add((x,y,z,w))

        state = new_state
    return state

def solveA(state):
    new_state = do_cycle(state, 6)
    print("Part 1:", len(new_state))

def solveB(state):
    new_state = do_cycle_4d(state, 6)
    print("Part 2:", len(new_state))

if __name__ == "__main__":
    state = set()

    with open('input.txt') as f:
        y = 0
        for l in f:
            for x, c in enumerate(l.strip()):
                if c == '.':
                    continue
                state.add((x, y, 0, 0))
            y += 1

    solveA(state)
    solveB(state)
