// Advent of Code 2020
// Peter Westerström (digimatic)

#include "config.h"

#include <common/common.h>

#include <algorithm>
#include <cassert>
#include <deque>
#include <functional>
#include <iostream>
#include <regex>
#include <stdexcept>
#include <string>
#include <tuple>
#include <unordered_map>
#include <unordered_set>
#include <utility>

using namespace westerstrom;
using namespace std;
using namespace std::string_literals;

int parseLine(const string& line)
{
	auto remaining = line;
	size_t taken = 0;
	auto n = std::stoi(remaining, &taken);
	return n;

/*
	regex re("(\\w+)\\)(\\w+)");
	smatch m;
	regex_match(line, m, re);
	return {m[1].str(), m[2].str()};
*/
}

vector<int> parseLines(const vector<string>& lines)
{
	vector<int> parsedLines;
	for(auto& line : lines)
	{
		parsedLines.push_back(parseLine(line));
	}
	return parsedLines;
}

void solve_part1()
{
	auto parsedInput = parseLines(readLines(string(inputFile)));
	for(auto x : parsedInput)
	{
	}

	cout << dayName << " - part 1: " << endl;
}

void solve_part2()
{
	cout << dayName << " - part 2: " << endl;
}

int main()
{
	solve_part1();
	solve_part2();
	return 0;
}
