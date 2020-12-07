// Advent of Code 2020
// Peter Westerström (digimatic)

#include "config.h"

#include <common/common.h>

#include <algorithm>
#include <deque>
#include <functional>
#include <iostream>
#include <regex>
#include <set>
#include <string>
#include <unordered_map>
#include <utility>

#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

using namespace westerstrom;
using namespace std;
using namespace std::string_literals;
using std::filesystem::path;

// Part 1 & 2 shared -------------------------------------------------------------------------------

pair<string, vector<pair<int, string>>> parseLine(const string& line)
{
	regex re("(\\w+ \\w+) bags contain (.*)");
	smatch m;
	regex_match(line, m, re);
	auto p = m[1].str();
	auto s = m[2].str();

	if(s == "no other bags.")
	{
		return {p, {}};
	}

	auto childrenWithCount = split(regex("\\s*[,.]\\s*"), s);
	vector<pair<int, string>> children;
	for(auto c : childrenWithCount)
	{
		regex re("(\\d) (\\w+ \\w+) bag(s?)");
		smatch m;
		regex_match(c, m, re);
		auto n = stoi(m[1].str());
		auto col = m[2].str();

		children.push_back(make_pair(n, col));
	}
	return {p, children};
}

// Part 1 ------------------------------------------------------------------------------------------

using Graph = std::unordered_map<string, std::set<string>>;

auto parseLines(const vector<string>& lines)
{
	Graph g;
	vector<decltype(parseLine(declval<string>()))> parsedLines;
	for(auto& line : lines)
	{
		auto [p, cs] = parseLine(line);
		for(auto [_, c] : cs)
		{
			auto& xs = g[c];
			xs.insert(p);
		}
	}
	return g;
}

auto solve_part1(const path& inputFile)
{
	auto g = parseLines(readLines(string(inputFile)));

	int n = 0;
	deque<string> q;
	set<string> visited;
	q.push_back("shiny gold");
	while(!q.empty())
	{
		auto c = q.front();
		q.pop_front();
		if(visited.contains(c))
			continue;
		visited.insert(c);

		++n;
		for(auto next : g[c])
		{
			q.push_back(next);
		}
	}
	return n - 1;
}

// Part 2 ------------------------------------------------------------------------------------------

using Graph2 = std::unordered_map<string, vector<pair<int, string>>>;

auto parseLines2(const vector<string>& lines)
{
	Graph2 g;
	vector<decltype(parseLine(declval<string>()))> parsedLines;
	for(auto& line : lines)
	{
		auto [p, cs] = parseLine(line);
		for(auto pc : cs)
		{
			g[p].push_back(pc);
		}
	}
	return g;
}

int count(Graph2& g, string current)
{
	int total{0};
	for(auto [n, next] : g[current])
	{
		total += n + n * count(g, next);
	}
	return total;
}

auto solve_part2(const path& inputFile)
{
	auto g = parseLines2(readLines(string(inputFile)));
	return count(g, "shiny gold");
}

// Unit tests and main for part 1&2 ----------------------------------------------------------------

TEST_CASE("examples-part1", "[solve_part1]")
{
	REQUIRE(solve_part1(path(dataDir) / path("inputExample1.txt")) == 4);
}

TEST_CASE("examples-part2", "[solve_part2]")
{
	REQUIRE(solve_part2(path(dataDir) / path("inputExample1.txt")) == 32);
	REQUIRE(solve_part2(path(dataDir) / path("inputExample2.txt")) == 126);
}

int main(int argc, char* argv[])
{
	int result = Catch::Session().run(argc, argv);
	cout << dayName << " - part 1: " << solve_part1(path(dataDir) / path("input.txt")) << "\n";
	cout << dayName << " - part 2: " << solve_part2(path(dataDir) / path("input.txt")) << "\n";
	return result;
}
