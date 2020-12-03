#!/usr/bin/env python3

from helpfunctions import *
import unittest, sys
from collections import namedtuple

MapPosition = namedtuple('MapPosition', ['x', 'y', 'tree'])

def getNextMapPosition(worldmap, currentPositition, deltaX, deltaY):
    # Raises IndexError when the end of the map is reached
    newX = currentPositition.x + deltaX
    if newX >= len(worldmap[0]):
        newX -= len(worldmap[0])
    newY = currentPositition.y + deltaY
    return MapPosition(newX, newY, worldmap[newY][newX] == '#')

def traverseMap(worldmap, deltaX, deltaY):
    position = MapPosition(0, 0, worldmap[0][0] == '#')
    numberOfTrees = 0
    try:
        while True:
            if position.tree:
                numberOfTrees += 1
            position = getNextMapPosition(worldmap, position, deltaX, deltaY)
    finally:
        return numberOfTrees

@timing
def part1(data):
    return traverseMap(list(data), 3, 1)

@timing
def part2(data):
    return 0

## Unit tests ########################################################

class TestDayXX(unittest.TestCase):
    worldmap = ["..##.......",
                "#...#...#..",
                ".#....#..#.",
                "..#.#...#.#",
                ".#...##..#.",
                "..#.##.....",
                ".#.#.#....#",
                ".#........#",
                "#.##...#...",
                "#...##....#",
                ".#..#...#.#"]

    def testGetNextMapPosition_1_1(self):
        currentPosition = MapPosition(1, 1, False)
        expectedPosition = MapPosition(2, 2, False)
        self.assertEqual(getNextMapPosition(self.worldmap, currentPosition, 1, 1),
                         expectedPosition)

    def testGetNextMapPosition_1_2(self):
        currentPosition = MapPosition(1, 1, False)
        expectedPosition = MapPosition(2, 3, True)
        self.assertEqual(getNextMapPosition(self.worldmap, currentPosition, 1, 2),
                         expectedPosition)

    def testGetNextMapPosition_2_1(self):
        currentPosition = MapPosition(1, 1, False)
        expectedPosition = MapPosition(3, 2, False)
        self.assertEqual(getNextMapPosition(self.worldmap, currentPosition, 2, 1),
                         expectedPosition)

    def testGetNextMapPosition_2_2(self):
        currentPosition = MapPosition(1, 1, False)
        expectedPosition = MapPosition(3, 3, False)
        self.assertEqual(getNextMapPosition(self.worldmap, currentPosition, 2, 2),
                         expectedPosition)

    def testGetNextMapPosition_overEdge(self):
        currentPosition = MapPosition(9, 3, False)
        expectedPosition = MapPosition(1, 4, True)
        self.assertEqual(getNextMapPosition(self.worldmap, currentPosition, 3, 1),
                         expectedPosition)

    def testTraverseMap(self):
        self.assertEqual(traverseMap(self.worldmap, 3, 1), 7)

    def test_part1(self):
        self.assertEqual(part1(self.worldmap), 7)

## Main ########################################################

if __name__ == '__main__':

    print("Advent of code day X")
    print("Part1 result: {}".format(part1(readFile(sys.argv[1]))))
    print("Part2 result: {}".format(part2(readFile(sys.argv[1]))))
