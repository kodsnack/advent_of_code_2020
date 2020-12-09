// Advent of Code 2020
// Peter Westerström (digimatic)

#include "config.h"

#include <common/common.h>

#include <algorithm>
#include <cstdint>
#include <filesystem>
#include <iostream>
#include <optional>
#include <stddef.h>
#include <string>
#include <string_view>
#include <type_traits>
#include <utility>
#include <vector>

#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

using namespace westerstrom;
using namespace std;
using namespace std::string_literals;
using std::filesystem::path;

int64_t parseLine(const string& line)
{
	auto remaining = line;
	size_t taken = 0;
	auto n = std::stoll(remaining, &taken);
	return n;
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

bool checkTwo(const vector<int64_t>& d, int n, int i)
{
	for(int j = i - n; j < i; ++j)
	{
		for(int k = i - n; k < j; ++k)
		{
			if(d[j] + d[k] == d[i])
				return true;
		}
	}
	return false;
}

int64_t findFirstNonPairSum(const vector<int64_t>& d, int n)
{
	int i = n;
	for(; i < d.size(); ++i)
	{
		if(!checkTwo(d, n, i))
		{
			return d[i];
		}
	}
	return -1;
}

auto solve_part1(const path& inputFile, int n = 25)
{
	auto d = parseLines(readLines(string(inputFile)));
	return findFirstNonPairSum(d, n);
}

int64_t solve_part2(const path& inputFile, int n = 25)
{
	auto d = parseLines(readLines(string(inputFile)));
	auto i = findFirstNonPairSum(d, n);
	for(int j = 0; j < d.size(); ++j)
	{
		int64_t sum{};
		int64_t smallest = d[j];
		int64_t largest = d[j];
		for(int k = j + 1; k < d.size(); ++k)
		{
			sum += d[k];
			smallest = min(smallest, d[k]);
			largest = max(largest, d[k]);
			if(d[j] + sum == i)
			{
				return smallest + largest;
			}
		}
	}
	return -1;
}

TEST_CASE("examples-part1", "[solve_part1]")
{
	REQUIRE(solve_part1(path(dataDir) / path("inputExample1.txt"), 5) == 127);
}

TEST_CASE("examples-part2", "[solve_part2]")
{
	REQUIRE(solve_part2(path(dataDir) / path("inputExample1.txt"), 5) == 62);
}

int main(int argc, char* argv[])
{
	int result = Catch::Session().run(argc, argv);

	cout << dayName << " - part 1: " << solve_part1(path(dataDir) / path("input.txt")) << "\n";
	cout << dayName << " - part 2: " << solve_part2(path(dataDir) / path("input.txt")) << "\n";
	return result;
}
