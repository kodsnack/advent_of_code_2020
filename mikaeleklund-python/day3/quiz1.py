def main():
    matrix = []
    list = []
    count = 0
    lenrow = 0
    lencol = 0
    xcord = 0
    ycord = 0
    file = open("input.txt", "r")
    for row in file:
        row = row.strip()
        for char in row:
            list.append(char.strip())
        matrix.append(list)
        list = []
    lencol = len(matrix[0])-1
    lenrow = len(matrix)-1
    while ycord < lenrow:
        xcord += 3
        ycord += 1
        if xcord - lencol > 0:
            xcord = xcord - lencol - 1
        #print(matrix[ycord][xcord], xcord, ycord)
        #print(xcord)
        if (matrix[ycord][xcord]) == "#":
            count += 1
    print(count)

if __name__ == "__main__":

    main()