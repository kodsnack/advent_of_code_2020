def solve(lines):
#    lines = ['L.LL.LL.LL','LLLLLLL.LL','L.L.L..L..','LLLL.LL.LL','L.LL.LL.LL','L.LLLLL.LL','..L.L.....','LLLLLLLLLL','L.LLLLLL.L','L.LLLLL.LL']
    seats = [[c for c in x.strip()] for x in lines]

    changed = True
    while changed:
        changed = False
        cache = {}
        newseats = []
        for r in range(len(seats)):
            newline = []
            for c in range(len(seats[r])):
                seat = seats[r][c]
                newseat = ''
                around = sum([look(seats, c, r, x, y, cache) for x in (-1,0,1) for y in (-1,0,1)])
                if seat == '.':
                    newseat = '.'
                elif seat == 'L':
                    if around == 0:
                        newseat = '#'
                        changed = True
                    else:
                        newseat = 'L'
                elif seat == '#':
                    if around > 4:
                        newseat = 'L'
                        changed = True
                    else:
                        newseat = '#'
                newline.append(newseat)

            newseats.append(newline)
        seats = newseats
    return ''.join([''.join(r) for r in seats]).count('#')

def look(seats, x, y, dx, dy, cache):
    if (x, y, dx, dy) in cache: 
        return cache[(x, y, dx, dy)]
    elif ((dx == 0 and dy == 0) or
          x+dx < 0 or 
          y+dy < 0 or 
          x+dx >= len(seats[0]) or 
          y+dy >= len(seats) or 
          seats[y+dy][x+dx] == 'L'):
        return 0
    elif seats[y+dy][x+dx] == '#':
        return 1
    elif seats[y+dy][x+dx] == '.':
        cache[(x, y, dx, dy)] = look(seats, x+dx, y+dy, dx, dy, cache)
        return cache[(x, y, dx, dy)]


if __name__ == '__main__':
    lines = []
    with open('11.txt') as f:
        for line in f.readlines():
            lines.append(line)
    print(solve(lines))
