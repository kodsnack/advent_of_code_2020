// Advent of Code 2020
// Peter Westerström (digimatic)

#include "config.h"

#include <common/common.h>

#include <filesystem>
#include <iostream>
#include <stdexcept>
#include <string>
#include <string_view>
#include <vector>

#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

using namespace westerstrom;
using namespace std;
using namespace std::string_literals;
using std::filesystem::path;

using Map = vector<string>;

auto getAt(const Map& m, int y, int x)
{
	auto h = m.size();
	auto w = m[0].size();
	if(y < 0 || x < 0 || x >= w || y >= h)
		return '.';
	return m[y][x];
}

int countTotalOccupied(const Map& m)
{
	int count{0};
	auto h = m.size();
	auto w = m[0].size();
	for(int y = 0; y < h; ++y)
	{
		for(int x = 0; x < w; ++x)
		{
			if(getAt(m, y, x) == '#')
				++count;
		}
	}
	return count;
}

// --- Part 1 ---

int countAdj(const Map& m, int y, int x, char c)
{
	int n{};
	for(int dy = -1; dy <= 1; dy++)
	{
		for(int dx = -1; dx <= 1; dx++)
		{
			if(dx == 0 && dy == 0)
				continue;
			if(getAt(m, y + dy, x + dx) == c)
				n++;
		}
	}
	return n;
}

Map next(Map m0)
{
	auto m1 = m0;
	auto h = m0.size();
	auto w = m0[0].size();
	for(int y = 0; y < h; ++y)
	{
		for(int x = 0; x < w; ++x)
		{
			auto c = getAt(m0, y, x);
			if(c == 'L')
			{
				auto n = countAdj(m0, y, x, '#');
				if(n == 0)
				{
					m1[y][x] = '#';
				}
			} else if(c == '#')
			{
				auto n = countAdj(m0, y, x, '#');
				if(n >= 4)
				{
					m1[y][x] = 'L';
				}
			}
		}
	}
	return m1;
}

auto solve_part1(const path& inputFile)
{
	auto m = readLines(inputFile);
	auto h = m.size();
	auto w = m[0].size();
	while(true)
	{
		auto n = next(m);
		if(n == m)
			break;
		m = n;
	}
	return countTotalOccupied(m);
}

// --- Part 2 ---

int countLongAt(const Map& m, int y, int x, char c)
{
	int n{};
	for(int dy = -1; dy <= 1; dy++)
	{
		for(int dx = -1; dx <= 1; dx++)
		{
			if(dx == 0 && dy == 0)
				continue;
			int l = 1;
			while(getAt(m, y + dy * l, x + dx * l) == '.' && l < m.size())
				l++;
			if(getAt(m, y + dy * l, x + dx * l) == c)
				n++;
		}
	}
	return n;
}

Map nextp2(Map m0)
{
	auto m1 = m0;
	auto h = m0.size();
	auto w = m0[0].size();
	for(int y = 0; y < h; ++y)
	{
		for(int x = 0; x < w; ++x)
		{
			auto c = getAt(m0, y, x);
			auto n = countLongAt(m0, y, x, '#');
			if(c == 'L')
			{
				if(n == 0)
				{
					m1[y][x] = '#';
				}
			} else if(c == '#')
			{
				if(n >= 5)
				{
					m1[y][x] = 'L';
				}
			}
		}
	}
	return m1;
}

auto solve_part2(const path& inputFile)
{
	auto m = readLines(inputFile);
	auto h = m.size();
	auto w = m[0].size();
	while(true)
	{
		auto n = nextp2(m);
		if(n == m)
			break;
		m = n;
	}
	return countTotalOccupied(m);
}

// --- Tests and main ---

TEST_CASE("examples-part1", "[solve_part1]")
{
	REQUIRE(solve_part1(path(dataDir) / path("inputExample1.txt")) == 37);
}

TEST_CASE("examples-part2", "[solve_part2]")
{
	REQUIRE(solve_part2(path(dataDir) / path("inputExample1.txt")) == 26);
}

int main(int argc, char* argv[])
{
	int result = Catch::Session().run(argc, argv);

	cout << dayName << " - part 1: " << solve_part1(path(dataDir) / path("input.txt")) << "\n";
	cout << dayName << " - part 2: " << solve_part2(path(dataDir) / path("input.txt")) << "\n";
	return result;
}
