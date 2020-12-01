// Advent of Code 2020
// Peter Westerström (digimatic)

#include "config.h"

#include <common/common.h>
#include <iostream>
#include <string>
#include <utility>

using namespace westerstrom;
using namespace std;

int parseLine(const string& line)
{
	auto remaining = line;
	size_t taken = 0;
	auto n = std::stoi(remaining, &taken);
	return n;
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
	auto v = parseLines(readLines(string(inputFile)));
	for(int i = 0; i < v.size(); ++i)
		for(int j = 0; j < v.size(); ++j)
		{
			if(i == j)
				continue;
			if(v[i] + v[j] == 2020)
			{
				auto r = v[i] * v[j];
				cout << dayName << " - part 1: " << r << endl;
				return;
			}
		}
}

void solve_part2()
{
	auto v = parseLines(readLines(string(inputFile)));
	for(int k = 0; k < v.size(); ++k)
		for(int i = 0; i < v.size(); ++i)
			for(int j = 0; j < v.size(); ++j)
			{
				if(i == j || j == k || i == k)
					continue;
				if(v[i] + v[j] + v[k] == 2020)
				{
					auto r = v[i] * v[j] * v[k];
					cout << dayName << " - part 2: " << r << endl;
					return;
				}
			}
}

int main()
{
	solve_part1();
	solve_part2();
	return 0;
}
