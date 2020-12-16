// Advent of Code 2020
// Peter Westerström (digimatic)

#include "config.h"

#include <common/common.h>

#include <algorithm>
#include <filesystem>
#include <iostream>
#include <iterator>
#include <regex>
#include <set>
#include <stdint.h>
#include <string>
#include <string_view>
#include <tuple>
#include <type_traits>
#include <vector>

#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

using namespace westerstrom;
using namespace std;
using namespace std::string_literals;
using std::filesystem::path;

using Rule = tuple<string, int, int, int, int>;

Rule parseRuleLine(const string& line)
{

	regex re("([^\\:]+)\\: (\\d+)-(\\d+) or (\\d+)-(\\d+)");
	smatch m;
	regex_match(line, m, re);
	return {m[1].str(), stoi(m[2]), stoi(m[3]), stoi(m[4]), stoi(m[5])};
}

auto parseLines(const vector<string>& lines)
{
	int step = 0;
	vector<Rule> rules;
	vector<int> yourTicket;
	vector<vector<int>> nearbyTickets;

	for(auto& line : lines)
	{
		if(line.empty())
		{
			step++;
		} else if(step == 0)
		{
			rules.push_back(parseRuleLine(line));
		} else if(step == 1)
		{
			step++;
		} else if(step == 2)
		{
			yourTicket = splitNumbers(line);
		} else if(step == 3)
		{
			step++;
		} else
		{
			nearbyTickets.push_back(splitNumbers(line));
		}
	}
	return make_tuple(rules, yourTicket, nearbyTickets);
}

bool isValid(int x, const Rule& rule)
{
	auto [_, a0, a1, b0, b1] = rule;
	return (((x >= a0 && x <= a1) || (x >= b0 && x <= b1)));
}

auto solve_part1(const path& inputFile)
{
	int invalidSum{0};
	auto d = parseLines(readLines(inputFile));
	for(auto& ticket : get<2>(d))
	{
		for(auto x : ticket)
		{
			bool valid{false};
			for(auto& rule : get<0>(d))
			{
				if(isValid(x, rule))
					valid = true;
			}
			if(!valid)
			{
				invalidSum += x;
			}
		}
	}

	return invalidSum;
}

int64_t solve_part2(const path& inputFile)
{
	auto [rules, myTicket, tickets] = parseLines(readLines(inputFile));
	int numRules = rules.size();
	for(int i = tickets.size() - 1; i >= 0; --i)
	{
		for(auto x : tickets[i])
		{
			bool valid{false};
			for(auto& rule : rules)
			{
				if(isValid(x, rule))
				{
					valid = true;
				}
			}
			if(!valid)
			{
				tickets.erase(begin(tickets) + i);
				break;
			}
		}
	}

	vector<set<int>> mapping;
	{
		set<int> ruleIndex;
		for(int i = 0; i < numRules; i++)
		{
			set<int> candidates;
			for(int j = 0; j < numRules; j++)
			{
				bool valid{true};
				for(auto t : tickets)
				{
					valid = valid && isValid(t[i], rules[j]);
					if(!valid)
						break;
				}
				if(valid)
				{
					candidates.insert(j);
				}
			}
			mapping.push_back(std::move(candidates));
		}
	}

	vector<int> finalMapping(numRules, -1);
	while(true)
	{
		auto it = find_if(begin(mapping), end(mapping), [](auto& s) { return s.size() == 1; });
		if(it != end(mapping))
		{
			auto x = *((*it).begin());
			int ii = distance(begin(mapping), it);
			finalMapping[ii] = x;

			for(int i = 0; i < numRules; i++)
			{
				mapping[i].erase(x);
			}
		} else
		{
			break;
		}
	}

	int64_t y = 1;
	int n{};
	for(int i = 0; i < 20; ++i)
	{
		auto& rule = rules[finalMapping[i]];
		if(get<0>(rule).starts_with("departure"))
		{
			y *= int64_t(myTicket[i]);
		}
	}
	return y;
}

TEST_CASE("examples-part1", "[solve_part1]")
{
	REQUIRE(solve_part1(path(dataDir) / path("inputExample1.txt")) == 71);
}

int main(int argc, char* argv[])
{
	int result = Catch::Session().run(argc, argv);

	cout << dayName << " - part 1: " << solve_part1(path(dataDir) / path("input.txt")) << "\n";
	cout << dayName << " - part 2: " << solve_part2(path(dataDir) / path("input.txt")) << "\n";
	return result;
}
