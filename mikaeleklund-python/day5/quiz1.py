def main():
    highest = 0
    file = open("input.txt", "r")
    for row in file:
        low=0
        high=127
        rlow=0
        rhigh=7
        delimiter=64
        rowdelimiter=4
        row = row.strip()
        if row != "":
            for item in row:  
                if item == "F":
                    high = int(high-delimiter)
                    low = int(high-delimiter+1)
                    delimiter = int(delimiter/2)
                elif item == "B":
                    low = int((high-delimiter+1))
                    high = int(low+delimiter-1)
                    delimiter = int(delimiter/2)
                elif item == "L":
                    rhigh = int(rhigh-rowdelimiter)
                    rlow = int(rhigh-rowdelimiter+1) 
                    rowdelimiter = int(rowdelimiter/2)
                elif item == "R":
                    rlow = int(rhigh-rowdelimiter+1)
                    rhigh = int(rlow+rowdelimiter-1)  
                    rowdelimiter = int(rowdelimiter/2)  
                else:
                    break
        if (high*8+rhigh)>highest:
            highest=(high*8+rhigh)
    print(highest)

if __name__ == "__main__":

    main()