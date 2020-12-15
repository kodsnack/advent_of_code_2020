// Advent of Code 2020
// Peter Westerström (digimatic)

#include "config.h"

#include <filesystem>
#include <iostream>
#include <type_traits>
#include <unordered_map>
#include <utility>
#include <vector>

#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

using namespace westerstrom;
using namespace std;
using std::filesystem::path;

auto solve(const vector<int>& input, int n)
{
	unordered_map<int, pair<int, int>> spoken;
	int turnIndex = 0;
	int lastSpoken = input.back();
	for(; turnIndex < input.size(); ++turnIndex)
	{
		//		cout << "Turn: " << (turnIndex+1) << " spoken number " << input[turnIndex] << endl;
		spoken[input[turnIndex]] = make_pair(0, turnIndex + 1);
		lastSpoken = input[turnIndex];
	}
	turnIndex++; // 1 based index instead from now on.
	for(; turnIndex <= n; turnIndex++)
	{
		auto [t1, t2] = spoken[lastSpoken];
		if(t1 == 0)
			lastSpoken = 0;
		else
			lastSpoken = t2 - t1;

		//		cout << "Turn: " << turnIndex << " spoken number " << lastSpoken << endl;
		auto& p = spoken[lastSpoken];
		std::swap(p.first, p.second);
		p.second = turnIndex;
	}
	return lastSpoken;
}

TEST_CASE("examples-part1", "[solve_part1]")
{
	REQUIRE(solve(vector<int>{0, 3, 6}, 2020) == 436);
}

TEST_CASE("examples-part2", "[solve_part2]")
{
	REQUIRE(solve(vector<int>{0, 3, 6}, 30000000) == 175594);
	REQUIRE(solve(vector<int>{1, 3, 2}, 30000000) == 2578);
}

int main(int argc, char* argv[])
{
	int result = Catch::Session().run(argc, argv);

	const vector<int> input = {5, 1, 9, 18, 13, 8, 0};
	cout << dayName << " - part 1: " << solve(input, 2020) << "\n";
	cout << dayName << " - part 2: " << solve(input, 30000000) << "\n";
	return result;
}
