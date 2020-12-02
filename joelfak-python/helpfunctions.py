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

def getIntsFromFile(filename):
    return map(lambda x: int(x), readFile(filename))
