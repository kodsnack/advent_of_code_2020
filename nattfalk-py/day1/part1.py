list_file = open("input.txt", "r")
lines = list_file.readlines()
list_file.close()

for x in lines:
    for y in reversed(lines):
        if int(x) + int(y) == 2020:
            break
    else:
        continue
    break

print(x.rstrip() + "*" + y.rstrip() + "=" + str(int(x)*int(y)))
