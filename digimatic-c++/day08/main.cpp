// Advent of Code 2020
// Peter Westerström (digimatic)

#include "config.h"

#include <common/common.h>

#include <algorithm>
#include <functional>
#include <iostream>
#include <string>
#include <unordered_set>
#include <utility>

#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

using namespace westerstrom;
using namespace std;
using namespace std::string_literals;
using std::filesystem::path;

pair<string, int> parseLine(const string& line)
{
	auto op = line.substr(0, 3);
	auto remaining = line.substr(4);
	size_t taken = 0;
	auto n = std::stoi(remaining, &taken);
	return {op, n};
}

auto parseLines(const vector<string>& lines)
{
	int i{};
	vector<decltype(parseLine(declval<string>()))> parsedLines;
	for(auto& line : lines)
	{
		auto [ip, o] = parseLine(line);
		parsedLines.push_back(make_pair(ip, o));
	}
	return parsedLines;
}

pair<int, bool> run(vector<pair<string, int>> p)
{
	int acc{};
	int ip{};
	unordered_set<int> visited;

	while(true)
	{
		if(visited.contains(p.size() - 1))
			return {acc, true};
		if(ip >= p.size())
			return {-1000, true};

		if(visited.contains(ip))
			return {acc, false};
		visited.insert(ip);
		auto [op, o] = p[ip];
		if(op == "acc")
		{
			acc += o;
			ip++;
		} else if(op == "jmp")
		{
			ip += o;
		} else if(op == "nop")
		{
			ip++;
		}
	}
}

auto solve_part1(const path& inputFile)
{
	auto p = parseLines(readLines(string(inputFile)));
	return run(p).first;
}

auto solve_part2(const path& inputFile)
{
	auto p = parseLines(readLines(string(inputFile)));
	for(int i = 0; i < p.size(); ++i)
	{
		auto p2 = p;
		if(p2[i].first == "nop")
		{
			p2[i].first = "jmp";
		} else if(p2[i].first == "jmp")
		{
			p2[i].first = "nop";
		} else
		{
			continue;
		}
		auto [acc, term] = run(p2);
		if(term)
			return acc;
	}
	return -100;
}

TEST_CASE("examples-part1", "[solve_part1]")
{
	REQUIRE(solve_part1(path(dataDir) / path("inputExample1.txt")) == 5);
}

TEST_CASE("examples-part2", "[solve_part2]")
{
	REQUIRE(solve_part2(path(dataDir) / path("inputExample1.txt")) == 8);
}

int main(int argc, char* argv[])
{
	int result = Catch::Session().run(argc, argv);

	cout << dayName << " - part 1: " << solve_part1(path(dataDir) / path("input.txt")) << "\n";
	cout << dayName << " - part 2: " << solve_part2(path(dataDir) / path("input.txt")) << "\n";
	return result;
}
