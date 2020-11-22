import sys
import os

def readInput(theTextFile=None):
	if theTextFile==None:
		thisFile = os.path.split(sys.argv[0])[1]
		thisFileBase = os.path.splitext(thisFile)[0]
		thisFileBase = thisFileBase.split("_")[0]
		theTextFile = thisFileBase+'.txt'
	with open(theTextFile) as f:
		s = f.read()
	return s