// Advent of Code 2020
// Peter Westerström (digimatic)

#include "config.h"

#include <common/common.h>

#include <algorithm>
#include <cstdint>
#include <filesystem>
#include <iostream>
#include <iterator>
#include <string>
#include <string_view>
#include <type_traits>
#include <vector>

#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

using namespace westerstrom;
using namespace std;
using std::filesystem::path;

auto parseLines(const vector<string>& lines)
{
	vector<int> parsedLines;
	for(auto& line : lines)
	{
		parsedLines.push_back(stoi(line));
	}
	return parsedLines;
}

auto solve_part1(const path& inputFile)
{
	auto ds = parseLines(readLines(inputFile));
	sort(begin(ds), end(ds));
	int c = 0;
	int dest = ds[ds.size() - 1];
	ds.push_back(dest + 3);
	int diff1count{};
	int diff3count{};
	for(int i = 0; i < ds.size(); i++)
	{
		int diff = ds[i] - c;
		if(diff == 1)
			diff1count++;
		else if(diff == 3)
			diff3count++;
		c = ds[i];
	}

	return diff1count * diff3count;
}

auto solve_part2(const path& inputFile)
{
	auto ds = parseLines(readLines(inputFile));
	ds.push_back(0);
	sort(begin(ds), end(ds));

	vector<int64_t> cs(ds.size());
	cs[0] = 1;
	for(int i = 0; i < ds.size(); i++)
	{
		for(int j = i + 1; j <= (i + 3) && j < ds.size(); j++)
		{
			auto diff = ds[j] - ds[i];
			if(diff <= 3)
				cs[j] += cs[i];
		}
	}
	return cs[cs.size() - 1];
}

TEST_CASE("examples-part1", "[solve_part1]")
{
	REQUIRE(solve_part1(path(dataDir) / path("inputExample1.txt")) == 7 * 5);
	REQUIRE(solve_part1(path(dataDir) / path("inputExample2.txt")) == 22 * 10);
}

TEST_CASE("examples-part2", "[solve_part2]")
{
	REQUIRE(solve_part2(path(dataDir) / path("inputExample1.txt")) == 8);
	REQUIRE(solve_part2(path(dataDir) / path("inputExample2.txt")) == 19208);
}

int main(int argc, char* argv[])
{
	int result = Catch::Session().run(argc, argv);
	cout << dayName << " - part 1: " << solve_part1(path(dataDir) / path("input.txt")) << "\n";
	cout << dayName << " - part 2: " << solve_part2(path(dataDir) / path("input.txt")) << "\n";
	return result;
}
