def solve(lines):
    inp = [int(x) for x in lines[0].split(',')]
    n = 0
    nums = {}
    last = 0
    for i in range(len(inp)):
        n += 1
        nums[inp[i]] = [n]
        last = inp[i]


    while n != 30000000:
        n += 1
        if len(nums[last]) == 1:
            new = 0
        else:
            new = nums[last][-1] - nums[last][-2]
        if new in nums:
            nums[new].append(n)
        else:
            nums[new] = [n]
            
        last = new

    return last

if __name__ == '__main__':
    lines = []
    with open('15.txt') as f:
        for line in f.readlines():
            lines.append(line)
    print(solve(lines))
