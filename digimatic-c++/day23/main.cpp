// Advent of Code 2020
// Peter Westerström (digimatic)

#include "config.h"

//#include <common/common.h>

#include <iostream>
#include <list>
#include <string>
#include <string_view>
#include <vector>

#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

using namespace westerstrom;
using namespace std;

int input = 157623984;

using Cups = list<int>;
using CupMap = vector<Cups::iterator>;

void playRound(Cups& cups, CupMap& cupMap, int n, Cups::iterator& ci)
{
	int c = *ci;

	auto ti = ci;
	ti++;
	if(ti == cups.end())
		ti = begin(cups);
	int c1 = *ti;
	ti = cups.erase(ti);
	if(ti == cups.end())
		ti = begin(cups);

	int c2 = *ti;
	ti = cups.erase(ti);
	if(ti == cups.end())
		ti = begin(cups);

	int c3 = *ti;
	ti = cups.erase(ti);

	int d = c - 1;
	if(d == 0)
		d = n;
	while(d == c1 || d == c2 || d == c3)
	{
		d--;
		if(d == 0)
			d = n;
	}

	auto dit = cupMap.at(d);
	dit++;
	if(dit == end(cups))
		dit = begin(cups);
	dit = cups.insert(dit, c3);
	cupMap[c3] = dit;
	dit = cups.insert(dit, c2);
	cupMap[c2] = dit;
	dit = cups.insert(dit, c1);
	cupMap[c1] = dit;

	ci++;
	if(ci == end(cups))
		ci = begin(cups);
}

void fill(Cups& cups, CupMap& cupMap, int c, int maxLabel)
{
	while(c != 0)
	{
		int d = c % 10;
		cups.push_front(d);
		cupMap[d] = cups.begin();
		c /= 10;
	}
	for(int i = 10; i <= maxLabel; i++)
	{
		cups.push_back(i);
		auto it = cups.end();
		--it;
		cupMap[i] = it;
	}
}

void play(Cups& cups, CupMap& cupMap, int maxLabel, int iterationCount)
{
	auto ci = cups.begin();
	for(int i = 0; i < iterationCount; i++)
	{
		playRound(cups, cupMap, maxLabel, ci);
	}
}

auto solve_part1(int input)
{
	Cups cups;
	CupMap cupMap(10);
	fill(cups, cupMap, input, 9);
	play(cups, cupMap, 9, 100);

	string s;
	auto it = cupMap.at(1);
	while(true)
	{
		++it;
		if(it == end(cups))
			it = begin(cups);
		if(*it == 1)
			break;
		s += ('0' + (*it));
	}
	return s;
}

auto solve_part2(int input, int maxLabel = 1000000, int iterationCount = 10000000)
{
	Cups cups;
	CupMap cupMap(maxLabel + 1);
	fill(cups, cupMap, input, maxLabel);
	play(cups, cupMap, maxLabel, iterationCount);

	auto it = cupMap.at(1);
	it++;
	if(it == end(cups))
		it = begin(cups);
	auto r1 = *it;
	++it;
	if(it == end(cups))
		it = begin(cups);
	auto r2 = *it;

	return int64_t(r1) * int64_t(r2);
}

TEST_CASE("examples-part1", "[solve_part1]")
{
	REQUIRE(solve_part1(389125467) == "67384529");
}

TEST_CASE("examples-part2", "[solve_part2]")
{
	REQUIRE(solve_part2(389125467) == 149245887792LL);
}

int main(int argc, char* argv[])
{
	int result = Catch::Session().run(argc, argv);

	cout << dayName << " - part 1: " << solve_part1(input) << "\n";
	cout << dayName << " - part 2: " << solve_part2(input) << "\n";
	return result;
}
