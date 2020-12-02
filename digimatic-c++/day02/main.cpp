// Advent of Code 2020
// Peter Westerström (digimatic)

#include "config.h"

#include <common/common.h>

#include <algorithm>
#include <iostream>
#include <regex>
#include <stdexcept>
#include <string>
#include <tuple>

using namespace westerstrom;
using namespace std;
using namespace std::string_literals;

tuple<int, int, char, string> parseLine(const string& line)
{
	regex re("(\\d+)\\-(\\d+) (\\w)\\: (\\w+)");
	smatch m;
	regex_match(line, m, re);
	auto m1 = m[1].str();
	auto m2 = m[2].str();
	auto m3 = m[3].str();
	auto m4 = m[4].str();
	return {stoi(m1), stoi(m2), m3[0], m4};
}

auto parseLines(const vector<string>& lines)
{
	vector<tuple<int, int, char, string>> parsedLines;
	for(auto& line : lines)
	{
		parsedLines.push_back(parseLine(line));
	}
	return parsedLines;
}

void solve_part1()
{
	auto parsedInput = parseLines(readLines(string(inputFile)));
	int numCorrect{};
	for(auto [x, y, c, s] : parsedInput)
	{
		int n{};
		for(auto d : s)
		{
			if(c == d)
				n++;
		}
		if(n >= x && n <= y)
			numCorrect++;
	}

	cout << dayName << " - part 1: " << numCorrect << endl;
}

void solve_part2()
{
	auto parsedInput = parseLines(readLines(string(inputFile)));
	int numCorrect{};
	for(auto [x, y, c, s] : parsedInput)
	{
		int n{};
		n += (s[x - 1] == c) ? 1 : 0;
		n += (s[y - 1] == c) ? 1 : 0;

		if(n == 1)
			numCorrect++;
	}
	cout << dayName << " - part 2: " << numCorrect << endl;
}

int main()
{
	solve_part1();
	solve_part2();
	return 0;
}
