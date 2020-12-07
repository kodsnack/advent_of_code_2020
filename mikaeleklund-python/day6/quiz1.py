def main():
    yesitems=[]
    count=0
    file = open("input.txt", "a+")
    file.write('\r\n \r\n')
    file.seek(0)
    for row in file:
        row = row.strip()
        if row != "":
            for item in row:  
                yesitems.append(item)
        else:
            yesitems = list(dict.fromkeys(yesitems))
            count+=len(yesitems)
            yesitems=[]  
    print(count)

if __name__ == "__main__":

    main()