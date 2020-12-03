// Advent of Code 2020
// Peter Westerström (digimatic)

#include "config.h"

#include <common/common.h>

#include <algorithm>
#include <iostream>
#include <string>

using namespace westerstrom;
using namespace std;
using namespace std::string_literals;

void solve_part1()
{
	auto d = readLines(string(inputFile));
	int x = 0, y = 0;
	int dx = 3;
	int dy = 1;
	int treeCount{};
	int w = (int)d[0].length();
	int h = (int)d.size();
	while(y < h)
	{
		if(d[y][x] == '#')
			treeCount++;
		x += dx;
		y += dy;
		x = x % w;
	}

	cout << dayName << " - part 1: " << treeCount << endl;
}

auto treeCounter(int dx, int dy)
{
	auto d = readLines(string(inputFile));
	int x = 0, y = 0;
	uint64_t treeCount{};
	int w = (int)d[0].length();
	int h = (int)d.size();
	while(y < h)
	{
		if(d[y][x] == '#')
			treeCount++;
		x += dx;
		y += dy;
		x = x % w;
	}
	return treeCount;
}

void solve_part2()
{
	auto n = treeCounter(1, 1) * treeCounter(3, 1) * treeCounter(5, 1) * treeCounter(7, 1) *
	         treeCounter(1, 2);
	cout << dayName << " - part 2: " << n << endl;
}

int main()
{
	solve_part1();
	solve_part2();
	return 0;
}
