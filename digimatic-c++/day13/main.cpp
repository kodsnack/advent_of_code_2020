// Advent of Code 2020
// Peter Westerström (digimatic)

#include "config.h"

#include <common/common.h>

#include <algorithm>
#include <cstdint>
#include <filesystem>
#include <iostream>
#include <iterator>
#include <regex>
#include <set>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

using namespace westerstrom;
using namespace std;
using std::filesystem::path;

auto parseLines(const vector<string>& lines)
{
	auto ts = stoi(lines[0]);
	auto items = split(regex(","), lines[1]);
	vector<int> d(items.size());
	transform(begin(items), end(items), begin(d), [](string s) {
		if(s == "x")
			return -1;
		else
			return stoi(s);
	});
	return make_pair(ts, d);
}

auto solve_part1(const path& inputFile)
{
	auto [ts, busses] = parseLines(readLines(inputFile));
	int selected{-1};
	int timeDelta{10000};
	int res{};
	for(auto id : busses)
	{
		if(id == -1)
			continue;
		auto delta = id - (ts % id);
		if(delta < timeDelta)
		{
			selected = id;
			timeDelta = delta;
			res = delta * id;
		}
	}
	return res;
}

auto findEarliest(const vector<int>& busses)
{
	vector<pair<int, int>> b;
	for(int i = 0; i < busses.size(); ++i)
	{
		auto busId = busses[i];
		if(busId != -1)
		{
			b.push_back(make_pair(busId, i));
			continue;
		}
	}

	int64_t ts{};
	int64_t stepSize{1};
	set<int64_t> stepSizes;
	bool found{};
	while(!found)
	{
		ts += stepSize;
		found = true;
		for(auto [id, offset] : b)
		{
			if(((ts + offset) % id) != 0)
			{
				found = false;
				break;
			} else if(!stepSizes.contains(id))
			{
				stepSizes.insert(id);
				stepSize = id * stepSize;
			}
		}
	}
	return ts;
}

auto solve_part2(const path& inputFile)
{
	auto [_, busses] = parseLines(readLines(inputFile));
	return findEarliest(busses);
}

TEST_CASE("examples-part1", "[solve_part1]")
{
	REQUIRE(solve_part1(path(dataDir) / path("inputExample1.txt")) == 295);
}

TEST_CASE("examples-part2", "[solve_part2]")
{
	REQUIRE(solve_part2(path(dataDir) / path("inputExample1.txt")) == 1068781);
	REQUIRE(findEarliest(vector<int>{67, 7, 59, 61}) == 754018);
	REQUIRE(findEarliest(vector<int>{67, -1, 7, 59, 61}) == 779210);
	REQUIRE(findEarliest(vector<int>{67, 7, -1, 59, 61}) == 1261476);
	REQUIRE(findEarliest(vector<int>{1789, 37, 47, 1889}) == 1202161486);
}

int main(int argc, char* argv[])
{
	int result = Catch::Session().run(argc, argv);

	cout << dayName << " - part 1: " << solve_part1(path(dataDir) / path("input.txt")) << "\n";
	cout << dayName << " - part 2: " << solve_part2(path(dataDir) / path("input.txt")) << "\n";
	return result;
}
