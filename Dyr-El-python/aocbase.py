import sys
import os

from functools import wraps
from time import time

def measure(func):
   @wraps(func)
   def _time_it(*args, **kwargs):
      start = int(round(time() * 1000))
      try:
         return func(*args, **kwargs)
      finally:
         end_ = int(round(time() * 1000)) - start
         print(f"Total execution time for {func.__name__}:\t{end_ if end_ > 0 else 0} ms")
   return _time_it

def readInput(theTextFile=None):
	if theTextFile==None:
		thisFile = os.path.split(sys.argv[0])[1]
		thisFileBase = os.path.splitext(thisFile)[0]
		thisFileBase = thisFileBase.split("_")[0]
		theTextFile = thisFileBase+'.txt'
	with open(theTextFile) as f:
		s = f.read()
	return s