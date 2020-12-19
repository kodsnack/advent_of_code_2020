// Advent of Code 2020
// Peter Westerström (digimatic)

#include "config.h"

#include <common/common.h>

#include <filesystem>
#include <iostream>
#include <set>
#include <stack>
#include <stddef.h>
#include <string>
#include <string_view>
#include <unordered_map>
#include <utility>
#include <vector>

#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

using namespace westerstrom;
using namespace std;
using std::filesystem::path;

struct Rule
{
	vector<int> seq1;
	vector<int> seq2;
	char c{'\0'};
};

pair<int, Rule> parseRule(const string& line)
{
	auto remaining = line;
	auto takews = [&remaining]() {
		while(!remaining.empty() && remaining[0] == ' ')
		{
			remaining = remaining.substr(1);
		}
	};

	size_t taken = 0;
	auto id = stoi(remaining, &taken);
	remaining = remaining.substr(taken + 1);
	takews();
	if(remaining[0] == '"')
	{
		Rule r;

		r.c = remaining[1];
		return {id, r};
	}
	Rule r;
	while(!remaining.empty() && remaining[0] != '|')
	{
		auto rid = stoi(remaining, &taken);
		remaining = remaining.substr(taken);
		takews();
		r.seq1.push_back(rid);
	}
	if(!remaining.empty())
	{
		remaining = remaining.substr(2);
		while(!remaining.empty())
		{
			auto rid = stoi(remaining, &taken);
			remaining = remaining.substr(taken);
			takews();
			r.seq2.push_back(rid);
		}
	}
	return {id, r};
}

auto parseLines(const vector<string>& lines)
{
	unordered_map<int, Rule> rules;
	vector<string> messages;

	int step{};
	for(auto& line : lines)
	{
		if(line.empty())
		{
			step++;
			continue;
		}
		if(step == 0)
		{
			auto [id, r] = parseRule(line);
			rules[id] = r;
		} else
		{
			messages.push_back(line);
		}
	}
	return make_pair(rules, messages);
}

// Part 1 checker.
int checkImpl(const unordered_map<int, Rule>& rules, const string& m, int i = 0, int rid = 0)
{
	auto& r = rules.at(rid);
	if(i >= m.size())
		return -1;

	if(r.c != '\0')
	{
		return (m[i] == r.c) ? 1 : -1;
	} else
	{
		auto j = 0;
		bool matched{true};

		for(int si = 0; si < r.seq1.size(); ++si)
		{
			auto res = checkImpl(rules, m, i + j, r.seq1[si]);
			if(res < 0)
			{
				matched = false;
				break;
			}
			j += res;
		}

		if(matched)
		{
			return j;
		}

		if(r.seq2.size() > 0)
		{
			j = 0;
			for(int si = 0; si < r.seq2.size(); ++si)
			{
				auto res = checkImpl(rules, m, i + j, r.seq2[si]);
				if(res < 0)
					return -1;
				j += res;
			}
			return j;
		}
		return -1;
	}
}

bool check(const unordered_map<int, Rule>& rules, const string& m)
{
	return checkImpl(rules, m) == m.length();
}

// Updated version for part 2 (can also solve part 1)
set<int> check2Impl(const unordered_map<int, Rule>& rules, const string& m, int i = 0, int rid = 0)
{
	auto& r = rules.at(rid);
	if(i >= m.size())
		return {};

	if(r.c != '\0')
	{
		return (m[i] == r.c) ? set<int>{1} : set<int>{};
	}

	set<int> matches;

	stack<pair<int, int>> s;
	s.push(make_pair(0, 0));
	while(!s.empty())
	{
		auto [si, j] = s.top();
		s.pop();

		if(si >= r.seq1.size())
		{
			matches.insert(j);
			continue;
		}

		auto res = check2Impl(rules, m, i + j, r.seq1[si]);
		for(auto x : res)
		{
			s.push(make_pair(si + 1, j + x));
		}
	}

	if(r.seq2.size() > 0)
	{
		s.push(make_pair(0, 0));
		while(!s.empty())
		{
			auto [si, j] = s.top();
			s.pop();

			if(si >= r.seq2.size())
			{
				matches.insert(j);
				continue;
			}

			auto res = check2Impl(rules, m, i + j, r.seq2[si]);
			for(auto x : res)
			{
				s.push(make_pair(si + 1, j + x));
			}
		}
	}
	return matches;
}

bool check2(const unordered_map<int, Rule>& rules, const string& m)
{
	auto vs = check2Impl(rules, m);
	for(auto v : vs)
	{
		if(v == m.length())
		{
			return true;
		}
	}
	return false;
}

int countMatches(const unordered_map<int, Rule>& rules, const vector<string>& messages,
                 int version = 1)
{
	int count{};
	for(auto m : messages)
	{
		if(version == 1 ? check(rules, m) : check2(rules, m))
		{
			count++;
		}
	}
	return count;
}

auto solve_part1(const path& inputFile)
{
	auto [rules, messages] = parseLines(readLines(inputFile));
	return countMatches(rules, messages, 1);
}

auto solve_part2(const path& inputFile)
{
	int count{};
	auto [rules, messages] = parseLines(readLines(inputFile));
	auto [r8id, r8] = parseRule("8: 42 | 42 8");
	auto [r11id, r11] = parseRule("11: 42 31 | 42 11 31");
	rules[8] = r8;
	rules[11] = r11;
	return countMatches(rules, messages, 2);
}

// -------------------------------------------------------------------------------------------------

TEST_CASE("examples-part1", "[solve_part1]")
{
	auto ex1 = path(dataDir) / path("inputExample1.txt");
	auto [rules, messages] = parseLines(readLines(ex1));
	REQUIRE(check(rules, "ababbb"));
	REQUIRE(check(rules, "abbbab"));

	REQUIRE(!check(rules, "bababa"));
	REQUIRE(!check(rules, "aaabbb"));
	REQUIRE(!check(rules, "aaaabbb"));

	REQUIRE(solve_part1(ex1) == 2);
}

TEST_CASE("examples-part1-check2", "[solve_part1]")
{
	auto ex1 = path(dataDir) / path("inputExample1.txt");
	auto [rules, messages] = parseLines(readLines(ex1));
	REQUIRE(check2(rules, "ababbb"));
	REQUIRE(check2(rules, "abbbab"));

	REQUIRE(!check2(rules, "bababa"));
	REQUIRE(!check2(rules, "aaabbb"));
	REQUIRE(!check2(rules, "aaaabbb"));
}

TEST_CASE("examples-part2", "[solve_part2]")
{
	auto ex1 = path(dataDir) / path("inputExample2.txt");
	auto [rules, messages] = parseLines(readLines(ex1));
	REQUIRE(check2(rules, "bbabbbbaabaabba"));

	REQUIRE(solve_part1(path(dataDir) / path("inputExample2.txt")) == 3);
	REQUIRE(solve_part2(path(dataDir) / path("inputExample2.txt")) == 12);
}

int main(int argc, char* argv[])
{
	int result = Catch::Session().run(argc, argv);

	cout << dayName << " - part 1: " << solve_part1(path(dataDir) / path("input.txt")) << "\n";
	cout << dayName << " - part 2: " << solve_part2(path(dataDir) / path("input.txt")) << "\n";
	return result;
}
