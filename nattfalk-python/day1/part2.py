list_file = open("input.txt", "r")
lines = list_file.readlines()
list_file.close()

for x in lines:
    for y in lines:
        for z in lines:
            if int(x) + int(y) + int(z) == 2020:
                break
        else:
            continue
        break
    else:
        continue
    break

print(x.rstrip() + "*" + y.rstrip() + "*" + z.rstrip() + "=" + str(int(x)*int(y)*int(z)))
