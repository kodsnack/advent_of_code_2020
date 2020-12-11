with open('input.txt') as f:
    jolts = sorted([int(l.strip()) for l in f])
jolts.append(max(jolts) + 3)

offset = 0
jolts_list = [0 for i in range(3+1)]
for j in jolts:
    diff = j - offset
    if (diff > 3): break
    jolts_list[diff] += 1
    offset += diff
print("Quiz 1: (%d*%d) = %d" % (jolts_list[1], jolts_list[3], jolts_list[1]*jolts_list[3]))

combinations = 1
offset = 0
ones = 0
for j in jolts:
    diff = j - offset
    if diff == 1:
        ones += 1
    else:
        if ones > 1:
            combinations *= (2 ** (ones - 1)) - sum([2**x for x in range(ones - 3)])
        ones = 0
    offset = j

print("Quiz 2: possible combinations %d" % combinations)
