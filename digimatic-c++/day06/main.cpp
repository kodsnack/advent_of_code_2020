// Advent of Code 2020
// Peter Westerström (digimatic)

#include "config.h"

#include <common/common.h>

#include <algorithm>
#include <functional>
#include <iostream>
#include <set>
#include <string>

#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

using namespace westerstrom;
using namespace std;
using namespace std::string_literals;
using std::filesystem::path;

auto solve_part1(const path& inputFile)
{
	auto lines = readLines(string(inputFile));
	int n{};
	set<char> current;
	for(auto& line : lines)
	{
		if(line.length() == 0)
		{
			n += current.size();
			current.clear();
		} else
		{
			current.insert(begin(line), end(line));
		}
	}
	n += current.size();
	return n;
}

auto solve_part2(const path& inputFile)
{
	auto lines = readLines(string(inputFile));
	int n{};
	set<char> current;
	bool first = true;
	for(auto& line : lines)
	{
		if(line.length() == 0)
		{
			n += current.size();
			current.clear();
			first = true;
		} else
		{
			set<char> c2;
			c2.insert(begin(line), end(line));

			if(first)
			{
				current = c2;
				first = false;
			} else
			{
				set<char> c3;
				set_intersection(begin(current), end(current), begin(c2), end(c2),
				                 std::inserter(c3, c3.begin()));
				current = c3;
			}
		}
	}
	n += current.size();
	return n;
}

TEST_CASE("examples-part1", "[solve_part1]")
{
	REQUIRE(solve_part1(path(dataDir) / path("inputExample1.txt")) == 11);
}

TEST_CASE("examples-part2", "[solve_part2]")
{
	REQUIRE(solve_part2(path(dataDir) / path("inputExample1.txt")) == 6);
}

int main(int argc, char* argv[])
{
	int result = Catch::Session().run(argc, argv);

	cout << dayName << " - part 1: " << solve_part1(path(dataDir) / path("input.txt")) << "\n";
	cout << dayName << " - part 2: " << solve_part2(path(dataDir) / path("input.txt")) << "\n";
	return result;
}
