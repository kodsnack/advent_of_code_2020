// Advent of Code 2020
// Peter Westerström (digimatic)

#include "config.h"

#include <common/common.h>
#include <common/tuple_hash.h>

#include <cassert>
#include <filesystem>
#include <iostream>
#include <iterator>
#include <numeric>
#include <stddef.h>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

using namespace westerstrom;
using namespace std;
using std::filesystem::path;

pair<int64_t, string> evalImp(const string& line, int version)
{
	auto remaining = line;

	int64_t val{};
	bool pendingOp{};
	bool pendingOpSum{true}; // else mul

	auto doPendingOp = [&val, &pendingOpSum, &pendingOp](int64_t val2) {
		if(pendingOp)
		{
			if(pendingOpSum)
				val = val + val2;
			else
				val = val * val2;
			pendingOp = false;
		} else
		{
			val = val2;
		}
	};

	while(!remaining.empty())
	{
		if(remaining[0] == ' ')
		{
			remaining = remaining.substr(1);
		}

		if(remaining[0] == ')')
		{
			return {val, remaining};
		} else if(remaining[0] == '(')
		{
			remaining = remaining.substr(1);
			auto [val2, remaining2] = evalImp(remaining, version);
			remaining = remaining2.substr(1);
			doPendingOp(val2);
		} else if(remaining[0] == '+')
		{
			remaining = remaining.substr(1);
			pendingOp = true;
			pendingOpSum = true;
		} else if(remaining[0] == '*')
		{
			remaining = remaining.substr(1);
			if(version == 1)
			{
				pendingOp = true;
				pendingOpSum = false;
			} else
			{
				remaining = remaining.substr(1);
				auto [val2, remaining2] = evalImp(remaining, version);
				remaining = remaining2;
				val = val * val2;
			}
		} else
		{
			size_t taken = 0;
			auto val2 = stoi(remaining, &taken);
			remaining = remaining.substr(taken);
			doPendingOp(val2);
		}
	}
	return {val, string()};
}

auto eval(const string& s, int version = 1)
{
	return evalImp(s, version).first;
}

auto eval2(const string& s)
{
	return evalImp(s, 2).first;
}

auto evalLines(const vector<string>& lines, int version = 1)
{
	vector<int64_t> parsedLines;
	for(auto& line : lines)
	{
		parsedLines.push_back(eval(line, version));
	}
	return parsedLines;
}

auto solve_part1(const path& inputFile)
{
	auto v = evalLines(readLines(inputFile));
	return accumulate(begin(v), end(v), 0LL);
}

auto solve_part2(const path& inputFile)
{
	auto v = evalLines(readLines(inputFile), 2);
	return accumulate(begin(v), end(v), 0LL);
}

TEST_CASE("examples-part1", "[solve_part1]")
{
	REQUIRE(eval("1 + 2 * 3 + 4 * 5 + 6") == 71);

	REQUIRE(eval("1 + (2 * 3) + (4 * (5 + 6))") == 51);

	REQUIRE(eval("2 * 3 + (4 * 5)") == 26);
	REQUIRE(eval("5 + (8 * 3 + 9 + 3 * 4 * 3)") == 437);
	REQUIRE(eval("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))") == 12240);
	REQUIRE(eval("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2") == 13632);
}

TEST_CASE("examples-part2", "[solve_part2]")
{
	REQUIRE(eval2("1 + 2 * 3 + 4 * 5 + 6") == 231);

	REQUIRE(eval2("1 + (2 * 3) + (4 * (5 + 6))") == 51);

	REQUIRE(eval2("2 * 3 + (4 * 5)") == 46);
	REQUIRE(eval2("5 + (8 * 3 + 9 + 3 * 4 * 3)") == 1445);
	REQUIRE(eval2("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))") == 669060);
	REQUIRE(eval2("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2") == 23340);
}

int main(int argc, char* argv[])
{
	int result = Catch::Session().run(argc, argv);

	cout << dayName << " - part 1: " << solve_part1(path(dataDir) / path("input.txt")) << "\n";
	cout << dayName << " - part 2: " << solve_part2(path(dataDir) / path("input.txt")) << "\n";
	return result;
}
