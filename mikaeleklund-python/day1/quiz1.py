#! /usr/bin/env python
"""
A skeleton python script which reads from an input file,
writes to an output file and parses command line arguments
"""
def main():
    items=[]
    file = open("input.txt", "r")
    for row in file:
        items.append(int(row.strip()))
    for counter1 in range (0, len(items)):
        for counter2 in range(counter1 + 1, len(items)-1):
            if items[counter1] + items[counter2] == 2020:
                print(items[counter1] * items[counter2] )
                exit()

if __name__ == "__main__":

    main()