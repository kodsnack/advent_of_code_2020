def solve(lines):
    bags = {i[0].strip(): [(k.strip().split()[0], " ".join(k.strip().split()[1:-1])) for k in i[1].split(',') if k.strip() != 'no other bags.'] for i in [j.split('bags contain') for j in lines]}

    '''
    more = True
    found_bags = []
    while more:
        more = False
        l = []
        if len(found_bags) == 0:
            l = find_bags('shiny gold', bags)
            if len(l) > 0: more = True
        else:
            for x in range(len(found_bags)):
                t = [y for y in find_bags(found_bags[x], bags) if y not in found_bags and y not in l]
                if len(t) > 0:
                    l.extend(t)
                    more = True
        found_bags.extend(l)
'''

    return count_bags('shiny gold', bags) - 1

def find_bags(bag, bags):
    res = [(k, [v[1] for v in bags[k]]) for k in bags]

    return [k[0] for k in res if bag in k[1]]

def count_bags(bag, bags):
    return 1 + sum([int(b[0]) * count_bags(b[1], bags) for b in bags[bag]])

if __name__ == '__main__':
    lines = []
    with open('7.txt') as f:
        for line in f.readlines():
            lines.append(line)
    print(solve(lines))
