def main():
    items=[]
    yesitems=[]
    count=0
    rowcount=0
    file = open("input.txt", "a+")
    file.write('\r\n \r\n')
    file.seek(0)
    for row in file:
        rowcount+=1
        row = row.strip()
        if row != "":
            for item in row:  
                items.append(item)
        else:
            for item in items:
                if items.count(item)==rowcount-1:
                    yesitems.append(item)
            yesitems = list(dict.fromkeys(yesitems))
            count+=len(yesitems)
            yesitems=[]  
            rowcount=0
            items=[]
    print(count)

if __name__ == "__main__":

    main()