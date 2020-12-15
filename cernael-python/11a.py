def solve(lines):
    seats = [[c for c in x.strip()] for x in lines]

    changed = True
    while changed:
        changed = False
        newseats = []
        for r in range(len(seats)):
            newline = []
            for c in range(len(seats[r])):
                seat = seats[r][c]
                newseat = ''
                around = ''.join([''.join(row[max(0,c-1):c+2]) for row in seats[max(0,r-1):r+2]])
                if seat == '.':
                    newseat = '.'
                elif seat == 'L':
                    if around.count('#') == 0:
                        newseat = '#'
                        changed = True
                    else:
                        newseat = 'L'
                elif seat == '#':
                    if around.count('#') > 4:
                        newseat = 'L'
                        changed = True
                    else:
                        newseat = '#'
                newline.append(newseat)

            newseats.append(newline)
        seats = newseats
    return ''.join([''.join(r) for r in seats]).count('#')

if __name__ == '__main__':
    lines = []
    with open('11.txt') as f:
        for line in f.readlines():
            lines.append(line)
    print(solve(lines))
