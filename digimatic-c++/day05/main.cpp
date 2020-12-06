// Advent of Code 2020
// Peter Westerström (digimatic)

#include "config.h"

#include <common/common.h>

#include <algorithm>
#include <iostream>
#include <set>
#include <string>
#include <utility>

#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

using namespace westerstrom;
using namespace std;
using namespace std::string_literals;
using std::filesystem::path;

pair<int, int> decodePos(string s)
{
	int a = 0;
	int b = 128;
	for(int i = 0; i < 7; ++i)
	{
		int c = a + (b - a) / 2;
		if(s[i] == 'F')
		{
			b = c;
		} else
		{
			a = c;
		}
	}
	int row = a;

	a = 0;
	b = 8;
	for(int i = 7; i < 10; ++i)
	{
		int c = a + (b - a) / 2;
		if(s[i] == 'L')
		{
			b = c;
		} else
		{
			a = c;
		}
	}

	return {row, a};
}

auto computeId(pair<int, int> p)
{
	auto [row, col] = p;
	auto id = row * 8 + col;
	return id;
}

auto solve_part1(const path& inputFile)
{
	auto d = readLines(string(inputFile));
	int maxId = -1;
	for(auto s : d)
	{
		auto id = computeId(decodePos(s));
		maxId = max(id, maxId);
	}

	return maxId;
}

auto solve_part2(const path& inputFile)
{
	set<pair<int, int>> seats;
	for(int row = 1; row < 127; row++)
	{
		for(int col = 0; col < 8; col++)
		{
			seats.insert(make_pair(row, col));
		}
	}

	auto d = readLines(string(inputFile));
	for(auto s : d)
	{
		auto pos = decodePos(s);
		seats.erase(pos);
	}

	for(auto p : seats)
	{
		auto [r, c] = p;
		if(seats.find(make_pair(r - 1, c)) == end(seats) &&
		   seats.find(make_pair(r + 1, c)) == end(seats))
		{
			return computeId(p);
		}
	}
	return -1;
}

TEST_CASE("check positions", "[decodePos]")
{
	REQUIRE(decodePos("FBFBBFFRLR") == make_pair(44, 5));
	REQUIRE(decodePos("BFFFBBFRRR") == make_pair(70, 7));
	REQUIRE(decodePos("FFFBBBFRRR") == make_pair(14, 7));
	REQUIRE(decodePos("BBFFBBFRLL") == make_pair(102, 4));
}

int main(int argc, char* argv[])
{
	int result = Catch::Session().run(argc, argv);

	cout << dayName << " - part 1: " << solve_part1(path(dataDir) / path("input.txt")) << "\n";
	cout << dayName << " - part 2: " << solve_part2(path(dataDir) / path("input.txt")) << "\n";
	return result;
}
