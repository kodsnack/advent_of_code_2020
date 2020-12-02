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
