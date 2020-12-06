def main():
    matrix = []
    list = []
    walk =[[1,1], [3,1], [5,1], [7,1],[1,2]]
    walkcount = 0
    count = 0
    sum = 1
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
    for cords in walk:
        while ycord < lenrow:
            xcord += cords[0]
            ycord += cords[1]
            if xcord - lencol > 0:
                xcord = xcord - lencol - 1
            if (matrix[ycord][xcord]) == "#":
                count += 1
        sum = count * sum
        count = 0
        xcord = 0
        ycord = 0
    print(sum)

if __name__ == "__main__":

    main()