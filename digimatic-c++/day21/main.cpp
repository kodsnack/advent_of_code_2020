// Advent of Code 2020
// Peter Westerström (digimatic)

#include "config.h"

#include <common/common.h>

#include <algorithm>
#include <cassert>
#include <filesystem>
#include <iostream>
#include <iterator>
#include <map>
#include <set>
#include <string>
#include <string_view>
#include <type_traits>
#include <utility>
#include <vector>

#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

using namespace westerstrom;
using namespace std;
using std::filesystem::path;

pair<vector<string>, vector<string>> parseLine(const string& line)
{
	vector<string> ingredients;
	int i = 0;
	while(auto j = line.find(' ', i))
	{
		ingredients.push_back(line.substr(i, j - i));
		i = j + 1;
		if(line[i] == '(')
			break;
	}
	i += 10;
	vector<string> allergens;
	while(auto j = line.find(',', i))
	{
		if(j == string::npos)
		{
			allergens.push_back(line.substr(i, line.size() - i - 1));
			break;
		}
		allergens.push_back(line.substr(i, j - i));
		i = j + 2;
	}

	return {ingredients, allergens};
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

pair<int, string> solve(const path& inputFile)
{
	auto foods = parseLines(readLines(inputFile));
	map<string, vector<set<string>>> ps;

	for(auto [ings, alls] : foods)
	{
		set<string> ingPoss;
		for(auto ing : ings)
		{
			ingPoss.insert(ing);
		}
		for(auto al : alls)
		{
			ps[al].push_back(ingPoss);
		}
	}

	for(auto& [all, p] : ps)
	{
		for(int i = p.size() - 1; i > 0; --i)
		{
			set<string> s;
			set_intersection(begin(p[0]), end(p[0]), begin(p[i]), end(p[i]),
			                 inserter(s, s.begin()));
			p[0] = s;
			p.erase(p.begin() + i);
		}
	}

	bool erasedOne = false;
	do
	{
		erasedOne = false;
		for(auto it1 = ps.begin(); it1 != ps.end(); ++it1)
		{
			string one;
			if(it1->second[0].size() == 1)
			{
				one = *(it1->second[0].begin());

				for(auto it2 = ps.begin(); it2 != ps.end(); ++it2)
				{
					if(it1 == it2)
						continue;
					if(it2->second[0].contains(one))
					{
						it2->second[0].erase(one);
						erasedOne = true;
					}
				}
			}
		}
	} while(erasedOne);
	// ps values now hopefully contain just one elment.

	set<string> ingredients;
	for(auto [ings, _] : foods)
	{
		for(auto ing : ings)
		{
			ingredients.insert(ing);
		}
	}

	for(auto& [all, p] : ps)
	{
		auto ing = *(p[0].begin());
		ingredients.erase(ing);
	}

	int n{}; // part1 result
	for(auto [ings, _] : foods)
	{
		for(auto ing : ings)
		{
			if(ingredients.contains(ing))
				n++;
		}
	}

	// part 2
	vector<string> dangs;
	for(auto [_, p] : ps)
	{
		assert(p[0].size() == 1);
		auto food = *p[0].begin();
		dangs.push_back(food);
	}
	string dangerous;
	for(int i = 0; i < dangs.size(); ++i)
	{
		dangerous += dangs[i];
		if(i != dangs.size() - 1)
		{
			dangerous += ",";
		}
	}

	return {n, dangerous};
}

int solve_part1(const path& inputFile)
{
	return solve(inputFile).first;
}

auto solve_part2(const path& inputFile)
{
	return solve(inputFile).second;
}

TEST_CASE("examples-part1", "[solve_part1]")
{
	REQUIRE(solve_part1(path(dataDir) / path("inputExample1.txt")) == 5);
}

TEST_CASE("examples-part2", "[solve_part2]")
{
	REQUIRE(solve_part2(path(dataDir) / path("inputExample1.txt")) == "mxmxvkd,sqjhc,fvjkl");
}

int main(int argc, char* argv[])
{
	int result = Catch::Session().run(argc, argv);

	cout << dayName << " - part 1: " << solve_part1(path(dataDir) / path("input.txt")) << "\n";
	cout << dayName << " - part 2: " << solve_part2(path(dataDir) / path("input.txt")) << "\n";
	return result;
}
