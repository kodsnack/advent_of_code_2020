def main():
    count = 0
    sum = 0
    file = open("input.txt", "r")
    for row in file:
        item = row.split(' ')
        low = int(item[0].split('-')[0])
        high = int(item[0].split('-')[1])
        letter = item[1][:-1]
        word = item[2].strip()
        count = word.count(letter)
        if count >= low and count <= high:
            sum += 1
    print(sum)
if __name__ == "__main__":

    main()