// Advent of Code 2020
// Peter Westerström (digimatic)

#include "config.h"

#include <common/common.h>
#include <common/tuple_math.h>

#include <algorithm>
#include <filesystem>
#include <iostream>
#include <set>
#include <string>
#include <string_view>
#include <tuple>
#include <type_traits>
#include <utility>
#include <vector>

#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

using namespace westerstrom;
using namespace std;
using std::filesystem::path;

using Point = tuple<int, int>;
using Dir = tuple<int, int>;
using Map = set<Point>;

Point parseLine(const string& line)
{
	int i = 0;
	vector<Dir> steps;
	Point p;

	while(i < line.length())
	{
		Dir d;
		switch(line[i])
		{
			case 'e':
				d = make_tuple(2, 0);
				break;
			case 's':
				i++;
				if(line[i] == 'w')
					d = make_tuple(-1, -1);
				else // se
					d = make_tuple(1, -1);
				break;
			case 'w':
				d = make_tuple(-2, 0);
				break;
			case 'n':
				i++;
				if(line[i] == 'w')
					d = make_tuple(-1, 1);
				else // ne
					d = make_tuple(1, 1);
				break;
		}
		p += d;
		i++;
	}
	return p;
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

void flip(Map& m, Point p)
{
	if(m.contains(p))
	{
		m.erase(p);
	} else
	{
		m.insert(p);
	}
}

auto solve_part1(const path& inputFile)
{
	auto ps = parseLines(readLines(inputFile));
	Map blackPoints;
	for(auto p : ps)
	{
		flip(blackPoints, p);
	}

	return blackPoints.size();
}

pair<Point, Point> getDimension(const Map& m)
{
	Point minP = *(m.begin());
	Point maxP = *(m.begin());
	for(auto [x, y] : m)
	{
		get<0>(minP) = min(get<0>(minP), x);
		get<1>(minP) = min(get<1>(minP), y);

		get<0>(maxP) = max(get<0>(maxP), x);
		get<1>(maxP) = max(get<1>(maxP), y);
	}
	return make_pair(minP, maxP);
}

const vector<Dir> neighs = {{-2, 0}, {2, 0}, {-1, 1}, {1, 1}, {-1, -1}, {1, -1}};

Map next(Map m)
{
	Map m2;
	auto [minP, maxP] = getDimension(m); // yes I know, not very optimal to do every time
	for(int y = get<1>(minP) - 1; y <= get<1>(maxP) + 1; ++y)
	{
		for(int x = get<0>(minP) - 2; x <= get<0>(maxP) + 2; ++x)
		{
			auto p = make_tuple(x, y);
			bool black = m.contains(p);
			int blackCount{};
			for(auto d : neighs)
			{
				auto p1 = p + d;
				if(m.contains(p1))
					blackCount++;
			}
			if(black)
			{
				if(blackCount == 0 || blackCount > 2)
				{
					black = false;
				}
			} else // white
			{
				if(blackCount == 2)
					black = true;
			}
			if(black)
				m2.insert(p);
		}
	}
	return m2;
}

auto solve_part2(const path& inputFile)
{
	auto ps = parseLines(readLines(inputFile));
	Map m;
	for(auto p : ps)
	{
		flip(m, p);
	}

	for(int i = 0; i < 100; ++i)
	{
		m = next(m);
	}

	return m.size();
}

TEST_CASE("examples-part1", "[solve_part1]")
{
	REQUIRE(solve_part1(path(dataDir) / path("inputExample1.txt")) == 10);
}

TEST_CASE("examples-part2", "[solve_part2]")
{
	REQUIRE(solve_part2(path(dataDir) / path("inputExample1.txt")) == 2208);
}

int main(int argc, char* argv[])
{
	int result = Catch::Session().run(argc, argv);

	cout << dayName << " - part 1: " << solve_part1(path(dataDir) / path("input.txt")) << "\n";
	cout << dayName << " - part 2: " << solve_part2(path(dataDir) / path("input.txt")) << "\n";
	return result;
}
