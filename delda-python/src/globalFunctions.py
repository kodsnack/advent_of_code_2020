import sys


def readInputFile(input_file=None):
    if input_file is None:
        print('Unable to fine input file!')
        sys.exit()

    fp = open(input_file)
    lines = fp.read().splitlines()
    fp.close()
    return lines
