// Advent of Code 2020
// Peter Westerström (digimatic)

#include "config.h"

#include <common/common.h>

#include <filesystem>
#include <iostream>
#include <stdlib.h>
#include <string>
#include <string_view>
#include <tuple>
#include <type_traits>
#include <utility>
#include <vector>

#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

using namespace westerstrom;
using namespace std;
using std::filesystem::path;

pair<char, int> parseLine(const string& line)
{
	char c = line[0];
	auto l = line.substr(1);
	auto n = stoi(l);
	return {c, n};
}

auto parseLines(const vector<string>& lines)
{
	vector<decltype(parseLine(declval<string>()))> parsedLines;
	for(auto& line : lines)
	{
		parsedLines.push_back(parseLine(line));
	}
	return parsedLines;
}

auto solve_part1(const path& inputFile)
{
	auto actions = parseLines(readLines(inputFile));
	tuple<int, int, int> state{0, 0, 0};
	for(auto [c, v] : actions)
	{
		auto [dir, x, y] = state;
		switch(c)
		{
			case 'N':
				state = make_tuple(dir, x, y + v);
				break;
			case 'S':
				state = make_tuple(dir, x, y - v);
				break;
			case 'E':
				state = make_tuple(dir, x + v, y);
				break;
			case 'W':
				state = make_tuple(dir, x - v, y);
				break;
			case 'R':
				dir -= v;
				if(dir < 0)
					dir += 360;
				state = make_tuple(dir, x, y);
				break;
			case 'L':
				state = make_tuple((dir + v) % 360, x, y);
				break;
			case 'F':
				switch(dir)
				{
					case 0:
						state = make_tuple(dir, x + v, y);
						break;
					case 90:
						state = make_tuple(dir, x, y + v);
						break;
					case 180:
						state = make_tuple(dir, x - v, y);
						break;
					case 270:
						state = make_tuple(dir, x, y - v);
						break;
				}
				break;
		}
	}
	auto [_, x, y] = state;
	return abs(x) + abs(y);
}

auto solve_part2(const path& inputFile)
{
	auto actions = parseLines(readLines(inputFile));
	tuple<int, int, int, int> state{0, 0, 10, 1};
	for(auto [c, v] : actions)
	{
		auto [x, y, wx, wy] = state;
		switch(c)
		{
			case 'N':
				state = make_tuple(x, y, wx, wy + v);
				break;
			case 'S':
				state = make_tuple(x, y, wx, wy - v);
				break;
			case 'E':
				state = make_tuple(x, y, wx + v, wy);
				break;
			case 'W':
				state = make_tuple(x, y, wx - v, wy);
				break;
			case 'R':
				switch(v)
				{
					case 0:
						break;
					case 90:
						state = make_tuple(x, y, wy, -wx);
						break;
					case 180:
						state = make_tuple(x, y, -wx, -wy);
						break;
					case 270:
						state = make_tuple(x, y, -wy, wx);
						break;
				}
				break;
			case 'L':
				switch(v)
				{
					case 0:
						break;
					case 90:
						state = make_tuple(x, y, -wy, wx);
						break;
					case 180:
						state = make_tuple(x, y, -wx, -wy);
						break;
					case 270:
						state = make_tuple(x, y, wy, -wx);
						break;
				}
				break;
			case 'F':
				state = make_tuple(x + wx * v, y + wy * v, wx, wy);
				break;
		}
	}
	auto [x, y, _, __] = state;
	return abs(x) + abs(y);
}

TEST_CASE("examples-part1", "[solve_part1]")
{
	REQUIRE(solve_part1(path(dataDir) / path("inputExample1.txt")) == 25);
}

TEST_CASE("examples-part2", "[solve_part2]")
{
	REQUIRE(solve_part2(path(dataDir) / path("inputExample1.txt")) == 286);
}

int main(int argc, char* argv[])
{
	int result = Catch::Session().run(argc, argv);

	cout << dayName << " - part 1: " << solve_part1(path(dataDir) / path("input.txt")) << "\n";
	cout << dayName << " - part 2: " << solve_part2(path(dataDir) / path("input.txt")) << "\n";
	return result;
}
