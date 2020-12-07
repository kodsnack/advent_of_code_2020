#!/usr/bin/env python3

from functools import wraps
from time import time
import unittest

def timing(f):
    @wraps(f)
    def wrap(*args, **kw):
        ts = time()
        result = f(*args, **kw)
        te = time()
        print('Execution of:%r took: %2.4f sec' % \
          (f.__name__, te-ts))
        return result
    return wrap

def readFile(filename):
    with open(filename) as file:
        while True:
            data = file.readline()
            if not data:
                break
            yield data.strip()

def parseInts(data):
    return map(lambda line: int(line), data)

def getIntsFromFile(filename):
    return parseInts(readFile(filename))

def parseTuples(data):
    return(line.split(': ') for line in data)

def getTuplesFromFile(filename):
    return parseTuples(readFile(filename))

def parseLineGroups(data, separator=' '):
    lineGroup = ""
    lineGroups = []
    for line in data:
        if line != "":
            lineGroup = separator.join([lineGroup, line])
        else:
            lineGroup = lineGroup.strip(separator)
            lineGroups.append(lineGroup)
            lineGroup = ""
    lineGroup = lineGroup.strip(separator)
    lineGroups.append(lineGroup)
    return lineGroups

def tryParseInt(s):
    if(s[0] == '0'):
        return s
    try:
        return int(s)
    except ValueError:
        return s

def parseDict(data):
    dicts = []
    for group in data:
        d = dict(x.split(":") for x in group.split(" "))
        for key, val in d.items():
            d[key] = tryParseInt(val)
        dicts.append(d)
    return dicts

def getDictsFromFile(filename):
    return parseDict(parseLineGroups(readFile(filename)))

## Unit tests ########################################################

class TestHelpFunctions(unittest.TestCase):
    def testParseInts(self):
        fileData = ["1535", "1908", "1783"]
        expectedRes = [1535, 1908, 1783]
        self.assertEqual(list(parseInts(line for line in fileData)), expectedRes)

    def testParseTuples(self):
        fileData = ["9-12 q: qqqxhnhdmqqqqjz", "12-16 z: zzzzzznwlzzjzdzf", "4-7 s: sssgssw"]
        expectedRes = [["9-12 q", "qqqxhnhdmqqqqjz"], ["12-16 z", "zzzzzznwlzzjzdzf"], ["4-7 s", "sssgssw"]]
        self.assertEqual(list(parseTuples(line for line in fileData)), expectedRes)

    def testParseLineGroups(self):
        fileData = """ a b c
                       1 2 3
                       
                       d e f
                       
                       4 5 6
                       g h i
                       j k l """
        fileLines = [line.strip() for line in fileData.splitlines()]

        expectedRes = ["a b c,1 2 3", "d e f", "4 5 6,g h i,j k l"]
        self.assertEqual(parseLineGroups(fileLines, separator=','), expectedRes)

        expectedRes = ["a b c 1 2 3", "d e f", "4 5 6 g h i j k l"]
        self.assertEqual(parseLineGroups(fileLines, separator=' '), expectedRes)

    def testTryParseInt_int(self):
        self.assertEqual(tryParseInt('5'), 5)

    def testTryParseInt_str(self):
        self.assertEqual(tryParseInt('a'), 'a')

    def testTryParseInt_leadingZero(self):
        self.assertEqual(tryParseInt('02'), '02')

    def testParseDict(self):
        fileData = ["a:1 b:z c:3",
                    "a:4 d:01 e:h5"]

        expectedRes = [{'a':1, 'b':'z', 'c':3},
                       {'a':4, 'd':'01', 'e':'h5'}]

        self.assertEqual(parseDict(fileData), expectedRes)