from functools import reduce


def get_seat_status(s, x, y):
    if 0 <= x < seats_per_row and 0 <= y < len(s)//seats_per_row:
        if s[y*seats_per_row+x] == '#':
            return 1
    return 0


def get_visible_occupied_seats(s, x, y):
    step = [[0, -1], [1, -1], [1, 0], [1, 1],
            [0, 1], [-1, 1], [-1, 0], [-1, -1]]
    row_size = len(s)//seats_per_row
    hit_count = 0
    for xs, ys in step:
        x2 = x + xs
        y2 = y + ys
        while (0 <= x2 < seats_per_row and 0 <= y2 < row_size):
            seat = s[y2*seats_per_row+x2]
            if seat == "#":
                hit_count += 1
                break
            elif seat == "L":
                break
            x2 += xs
            y2 += ys

    return hit_count


def get_adjacents(s, x, y):
    adjacents = \
        get_seat_status(s, x-1, y-1) \
        + get_seat_status(s, x, y-1) \
        + get_seat_status(s, x+1, y-1) \
        + get_seat_status(s, x-1, y) \
        + get_seat_status(s, x+1, y) \
        + get_seat_status(s, x-1, y+1) \
        + get_seat_status(s, x, y+1) \
        + get_seat_status(s, x+1, y+1)
    return adjacents


def is_free(s, x, y):
    if s[y*seats_per_row+x] == ".":
        return False

    adjacents = get_adjacents(s, x, y)
    if s[y*seats_per_row+x] == "L" and adjacents == 0:
        return True
    return False


def is_free2(s, x, y):
    if s[y*seats_per_row+x] == ".":
        return False
    return get_visible_occupied_seats(s, x, y) == 0


def is_occupied(s, x, y):
    adjacents = get_adjacents(s, x, y)
    if s[y*seats_per_row+x] == "#" and adjacents >= 4:
        return True
    return False


def is_occupied2(s, x, y):
    if s[y*seats_per_row+x] == ".":
        return False
    return get_visible_occupied_seats(s, x, y) >= 5


def print_pass(s, c):
    print("Pass ", c)
    for y in range(len(s)//seats_per_row):
        print("  ", "".join(s[y*seats_per_row:(y+1)*seats_per_row]))


def get_occupied_seats(state, is_free_func, is_occupied_func,  print_progress):
    occupied_seats = 0
    do_occupy = False
    pass_count = 0
    while True:
        new_state = state[:]
        for y in range(len(new_state)//seats_per_row):
            for x in range(seats_per_row):
                if state[y*seats_per_row+x] == ".":
                    continue
                if do_occupy:
                    if is_free_func(state, x, y):
                        new_state[y*seats_per_row+x] = "#"
                else:
                    if is_occupied_func(state, x, y):
                        new_state[y*seats_per_row+x] = "L"

        state = new_state
        do_occupy = True if do_occupy == False else False

        cnt = reduce(lambda x, y: x+y, state).count('#')
        if cnt == occupied_seats:
            break
        occupied_seats = cnt

        if print_progress:
            pass_count += 1
            print_pass(state, pass_count)
    return occupied_seats


seats_per_row = 0
initial_state = []

with open('input.txt') as f:
    for l in f:
        row = [x.replace('L', '#') for x in l.strip()]
        if seats_per_row == 0:
            seats_per_row = len(row)
        initial_state.extend(row)

occupied_seats = get_occupied_seats(
    initial_state[:], is_free, is_occupied, False)
print("Part 1 - occupied seats:", occupied_seats)

occupied_seats = get_occupied_seats(
    initial_state[:], is_free2, is_occupied2, False)
print("Part 2 - occupied seats:", occupied_seats)
